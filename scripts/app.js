var app = angular.module("xinitrcde", ['ngRoute', 'ngAnimate', 'leaflet-directive']);

app.config(['$routeProvider', '$locationProvider', function($routeProvider, $locationProvider){
    $routeProvider.when('/', {
        templateUrl: function (params) {
            return '/views/index.html';
        }
    });
    $routeProvider.when('/:url', {
        templateUrl: function (params) {
            return '/views/' + params.url;
        }
    });
    $routeProvider.when('/tags/:tag', {
        templateUrl: function (params) {
            return '/views/tags/' + params.tag;
        }
    });
    $routeProvider.when('/:year/:month/:day/:title', {
        templateUrl: function (params) {
            return '/views/' + params.year + '/' + params.month + '/' + params.day + '/' + params.title;
        }
    });
    $locationProvider.html5Mode(true);
}]);

app.run(['$rootScope', '$timeout', '$window', function ($rootScope, $timeout, $window) {
    $rootScope.$on('$routeChangeSuccess', function () {
        $timeout(function () {
            $window.scrollTo(0,0);
        }, 500);
    });
}]);
