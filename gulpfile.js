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

gulp.task('default', ['style', 'script'], function () {

});

gulp.task('test', function() {

});

gulp.task('styleMerge', function() {
    return gulp.src(['static/assets/fonts/Fira/css/fira.css', 'css/style.scss', 'bower_components/mediaelement/build/mediaelementplayer.min.css', 'bower_components/leaflet-dist/leaflet.css'])
        .pipe(concat('complete.scss'))
        .pipe(gulp.dest('css/'))
        .pipe(notify({message: "Finished merging styles"}));
});

gulp.task('style', function() {
    return sass('css/complete.scss', {compass: true})
        .pipe(gulp.dest('css/complete.css'))
        .pipe(rename({suffix: '.min'}))
        .pipe(minifycss())
        .pipe(gulp.dest('css'))
        .pipe(notify({message: "Finished generating styles"}));
    });

gulp.task('script', function() {
    return gulp.src(['bower_components/jquery/dist/jquery.min.js','bower_components/angular/angular.min.js', 'bower_components/angular-route/angular-route.min.js','bower_components/angular-animate/angular-animate.min.js','bower_components/leaflet-dist/leaflet.js','bower_components/angular-leaflet/dist/angular-leaflet-directive.min.js','bower_components/modernizr/modernizr.js','bower_components/mediaelement/build/mediaelement-and-player.min.js','scripts/app.js','scripts/commonDirectives/prefetch.js','scripts/menu/menu.js','scripts/maincontent/mainContent.js','scripts/whereabouts/whereabouts.js'])
        .pipe(concat('app.js'))
        .pipe(rename({suffix: '.min'}))
        .pipe(uglify())
        .pipe(gulp.dest('scripts'))
        .pipe(notify({message: "Finished generating script"}));
});
