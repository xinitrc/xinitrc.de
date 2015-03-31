var app = angular.module("xinitrcde");

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