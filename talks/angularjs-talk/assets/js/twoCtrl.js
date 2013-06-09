var app = angular.module("Todo");

var twoCtrl = function ($scope) {
    $scope.tb3 ="";
};

twoCtrl.$inject = ['$scope'];

app.controller ("TwoCtrl", twoCtrl);

