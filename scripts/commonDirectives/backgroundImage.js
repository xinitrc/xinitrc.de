var app = angular.module("xinitrcde");

app.directive('backgroundImage', ["$window", function ($window) {
    return function (scope, element, attr) {
        var w = angular.element($window);
        scope.getWindowDimensions = function () {
            return w.width()
        };
        scope.$watch(scope.getWindowDimensions, function (newValue, oldValue) {
            scope.bgimagestyle = function () {
                if (newValue >= 960) {
                    return {
                        'background-image': 'url("' + attr.backgroundImage + '")'
                    }
                } else {
                    return {}
                }
            };

        }, true);

        w.bind('resize', function () {
            scope.$apply();
        });
    }
}]);