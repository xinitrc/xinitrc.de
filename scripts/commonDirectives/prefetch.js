var app = angular.module("xinitrcde");

app.directive('prefetch', [function () {
    return {
        link: function (scope, element, attr) {
            scope.preloadResource = element.html();
            element.removeAttr("prefetch");
        }
    }
}]);