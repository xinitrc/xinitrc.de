var app = angular.module('xinitrc', ["leaflet-directive"]);

var calendarCTRL = function ($scope, $http) {
    $scope.calendarEntries = [];

    var arr = [];

    $http.get('/data/calendar.json').success(function (data) {
        angular.forEach(data.items, function (d) {
            var datetimeFormat = "yyyy-mm-dd HH:MM";
            var dateFormat = "yyyy-mm-dd"

            var startTime = d.start.dateTime ? new Date(d.start.dateTime).format(datetimeFormat)
                : new Date(d.start.date).format(dateFormat);
            var endTime = d.end.dateTime ? new Date(d.end.dateTime).format(datetimeFormat)
                : new Date(d.end.date).format(dateFormat);
 
            arr.push({
                start: startTime,
                end: endTime,
                summary: d.summary,
                location: d.location,
                mapsURL:"http://maps.google.de/maps?q=" + encodeURIComponent(d.location)+ "&hl=en"
            });
        });

        arr.sort(function (a, b) {
            return Date.parse(a.start) - Date.parse(b.start);
        });

        $scope.calendarEntries = arr;

    }).error(function (error) {
            console.log(error)
        });
}

calendarCTRL.$inject = ['$scope', '$http'];

app.controller('CalendarCTRL', calendarCTRL);
