var app = angular.module('xinitrcde');

app.directive('articleintro', ['$window', '$document', '$timeout', '$log', function ($window, $document, $timeout, $log) {
    return {
        link: function (scope, element) {
            scope.showCss = "";

            noscroll = false;
            isRevealed = false;

            function scrollY() {
                return $window.pageYOffset
            }

            function scrollPage() {
                scrollVal = scrollY();

                if (noscroll) {
                    if (scrollVal < 0) {
                        return false
                    }
                    $window.scrollTo(0, 0);
                }
                if (scrollVal <= 0 && isRevealed) {
                    toggle(0);
                } else if (scrollVal > 0 && !isRevealed) {
                    toggle(1)
                }
            }

            function toggle(reveal) {
                if (reveal) {
                    scope.showCss = "show";
                } else {
                    noscroll = true;
                    disable_scroll();
                    scope.showCss = "";
                }

                $timeout(function () {
                    if (reveal) {
                        noscroll = false;
                        enable_scroll();
                    }
                }, 800)
            }

            function disable_scroll() {
                $window.onmousewheel = $document.onmousewheel = function (e) {}
            }

            function enable_scroll () {
                $window.onmousewheel = $document.onmousewheel = $document.onkeydown = null;
            }

            var pageScroll = scrollY();
            noscroll = pageScroll === 0;

            disable_scroll();

            if (pageScroll) {
                isRevealed = true;
            }

            //element.scroll(scrollPage);
            $window.addEventListener('scroll', scrollPage);
        }
    }
}]);