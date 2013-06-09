angular.module('Todo').directive('contenteditable', function () {
    return {
        require: 'ngModel',
        link: function (scope, elm, attrs, ctrl) {
            elm.bind('blur', function() {
                scope.$apply(function() {
                    ctrl.$setViewValue(elm.text());
                });
            });

            // model -> view
            ctrl.$render = function() {
                elm.html(ctrl.$viewValue);
            };

            // load init value from DOM
            //ctrl.$setViewValue(elm.html());
            ctrl.$render();
        }
    };
});