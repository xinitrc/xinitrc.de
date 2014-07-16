var app = angular.module("TestApp", []);

app.controller("menuCTRL", ["$scope", function ($scope) {
   var menuOpen = false;
    var selectedEntry = '';

    $scope.toggle = function () {
        menuOpen = !menuOpen;
    }

    $scope.closeMenu = function () {
        menuOpen = false;
    }

    $scope.open = function () {
        return menuOpen;
    }

    $scope.selected = function (name) {
        return selectedEntry === name || (name === "any" && selectedEntry !== '')
    }

    $scope.select = function (name) {
        selectedEntry = name;
    }
}]);

app.directive('backgroundImage', function ($window) {
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
});