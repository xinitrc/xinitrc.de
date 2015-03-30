/**
 * Created with JetBrains WebStorm.
 * User: martin
 * Date: 24.08.13
 * Time: 18:13
 * To change this template use File | Settings | File Templates.
 */

var app = angular.module('xinitrc');

var whereaboutCTRL = function ($scope, $http) {

    var calctime = function (t) {
        var now = new Date().getTime();

        var intervals = ['second', 'minute', 'hour', 'day'];
 	var divider = [60, 60, 60,24];
        var i = 0;
        var dist =  (now/ 1000) - t ;

        while (dist >= divider[i] && i < 3) {
            dist /= divider[i];
            i = i + 1;
        }

        var str = "Last seen here about " + Math.floor(dist) + " " + intervals[i];
        if (Math.floor(dist) !== 1) {
            str += "s"
        }
        str += " ago and mostly stationary since.";

        return str;
    }

    $http.get('/data/location.json').success(function (data) {
        if (data && data[0] && data[0].lon && data[0].lat && data[0].t) {
            $scope.center.lng = data[0].lon;
            $scope.center.lat = data[0].lat;
            $scope.markers.whereabouts.lng = data[0].lon;
            $scope.markers.whereabouts.lat = data[0].lat;

            $scope.message = calctime(data[0].t);
//            $scope.markers.whereabouts.message = calctime(data[0].t);
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
            }
        },
        defaults: {
            maxZoom: 16
        },
        layers: {
                    baselayers: {
                        stamen: {
                            name: 'Stamen',
                            type: 'xyz',
                            url: 'http://{s}.tile.stamen.com/toner/{z}/{x}/{y}.png'
                        }
                    }
                }
    });
}

whereaboutCTRL.$inject = ['$scope', '$http'];

app.controller('WhereaboutCTRL', whereaboutCTRL);
