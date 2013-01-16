window.russell_module = angular.module('russell', [])

russell_module.controller 'TileCtrl', () ->
    $(window).trigger 'resize'
