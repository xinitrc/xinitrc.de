var app = angular.module("xinitrcde");

app.controller("maincontentCTRL", ['$scope', '$log', function ($scope, $log) {
    $scope.showCss = "";

    $scope.show = function () {
        $log.log("Show");
        $scope.showCss = "show"
    }
}]);