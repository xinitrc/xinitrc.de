$(document).ready(function () {
    $('#menu-closer').click(function (e) {
        e.preventDefault();
        $('#sidebar').removeClass('open');
    });
    $('#menu-opener').click(function (e) {
        e.preventDefault();
        $('#sidebar').addClass('open');
    });
});
