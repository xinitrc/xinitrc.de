/**
 * Created with JetBrains WebStorm.
 * User: martin
 * Date: 24.08.13
 * Time: 18:13
 * To change this template use File | Settings | File Templates.
 */

var app = angular.module('Whereabouts', ["leaflet-directive"]);

var whereaboutCTRL = function ($scope, $http) {

    var calctime = function (t) {
        var now = new Date().getTime();

        var intervals = ['sec', 'min', 'hours'];
        var i = 0;
        var dist =  t - now / 1000;

        while (dist >= 60 && i < 3) {
            dist /= 60;
            i = i + 1;
        }

        return "Last seen here " + Math.floor(dist) + intervals[i] + "ago";
    }

    $http.get('/data/location.json').success(function (data) {
        if (data && data[0] && data[0].lon && data[0].lat && data[0].t) {
            $scope.center.lng = data[0].lon;
            $scope.center.lat = data[0].lat;
            $scope.markers.whereabouts.lng = data[0].lon;
            $scope.markers.whereabouts.lat = data[0].lat;
/*            $scope.markers.whereabouts.message = calctime(data[0].t); */m
        }
    });

        angular.extend($scope, {
        center: {
            lng: 8.176630973815918,
            lat: 53.143672943115234,
            zoom: 16
        },
        markers: {
            whereabouts: {
                lat: 0, /*53.143672943115234,*/
                lng: 0, /*8.176630973815918,*/
                message: "Last seen here",
                focus: true
            }
        },
        defaults: {
            maxZoom: 16
        }
    });
}

whereaboutCTRL.$inject = ['$scope', '$http'];

app.controller('WhereaboutCTRL', whereaboutCTRL);