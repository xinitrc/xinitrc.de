var app = angular.module('Calendar', []);

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
                summary: d.summary
            });
        });

        arr.sort(function (a, b) {
            a = new Date(a.start);
            b = new Date(b.start);
            return a < b ? -1 : a > b ? 1 : 0;
        });
        $scope.calendarEntries = arr;

    }).error(function (error) {
        console.log(error)
    });
}

calendarCTRL.$inject = ['$scope', '$http'];

app.controller('CalendarCTRL', calendarCTRL);
