window.russell_module = angular.module('russell', [])

russell_module.factory 'make_url', () -> (s) -> "http://#{window.location.host}" + s

russell_module.controller 'TileCtrl', () ->
    $(window).trigger 'resize'
