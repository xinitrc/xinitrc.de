/**
 * Created by martin on 18.08.14.
 */

"use strict";

var gulp = require('gulp'),
    sass = require('gulp-ruby-sass'),
    minifycss = require('gulp-minify-css'),
    concat = require('gulp-concat'),
    uglify = require('gulp-uglify'),
    clean = require('gulp-clean'),
    notify = require('gulp-notify'),
    rename = require('gulp-rename');

gulp.task('default', ['script'], function () {

});

gulp.task('script', function() {
    return gulp.src(['bower_components/jquery/dist/jquery.min.js', 'scripts/app.js'])
        .pipe(concat('app.js'))
        .pipe(rename({suffix: '.min'}))
        .pipe(uglify())
        .pipe(gulp.dest('scripts'))
        .pipe(notify({message: "Finished generating script"}));
});
