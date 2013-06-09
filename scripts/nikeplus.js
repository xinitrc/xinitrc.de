var nikeplusCallback = function () { 
    var nikeplus = '/data/np.json';
    var chart;
    $(document).ready(function () {
	var options = {
	    chart: {
		renderTo: 'nikeplus',
		animate: false
	    },
	    title: {
		text: null
	    },
	    xAxis: {
		categories: ['a', 'b', 'c', 'd', 'e'],
	    },
	    yAxis: [{
		title: {
		    text: 'Distance in (km)'
		}
	    }, {
		labels: {
		    formatter: function () {
			var val = this.value;
			return sprintf ("%d:%02d", Math.floor(val), + ((val * 10) % 10) * 6)
		    }
		},
		title: {
		    text: 'Speed (min/km)'
		},
		opposite: true
	    }],
	    tooltip: {
		formatter: function () {
		    var unit = {
			'Speed': 'min/km',
			'Distance': 'km'
		    }[this.series.name];
		    var ycomp = {
			'Speed': sprintf ("%d:%02d", Math.floor(this.y), ((this.y * 10) % 10) * 6),
			'Distance': this.y
		    }[this.series.name];
		    
		    return '' + ycomp + ' ' + unit;
		}
	    },
	    series: [{
		name: 'Distance',
		type: 'column',
		yAxis: 0
	    }, {
		name: 'Speed',
		type: 'spline',
		yAxis: 1
	    }]
	};
	
	$.get(nikeplus, null, function(o, state, xhr) {
	    var distances = [],
		speeds = [],
		rundates  = [];
	    
	    var arr = o.data.reverse();
	    
	    distances = $.map(arr, function (x, line) {
		return Math.round(x.metricSummary.distance * 100) / 100;
	    });
	    rundates = $.map(arr, function (x, line) {
		var d = new Date(x.startTime);
		return d.format("yyyy mmm dd");
	    });
	    speeds = $.map (arr, function (x, line) {
		var d = x.metricSummary.duration;
		var durationComponents = d.split(':');
		
		var speed = (parseInt(durationComponents[0]) * 60 + parseInt(durationComponents[1])) / (x.metricSummary.distance);
		speed = Math.round(speed * 100) / 100;
		return speed;
	    });
	    
	    options.series[0].data = distances;
	    options.series[1].data = speeds;
	    options.xAxis.categories = rundates;
	    
	    chart = new Highcharts.Chart(options);
	}, 'json');
    });
}
