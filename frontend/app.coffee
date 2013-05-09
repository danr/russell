window.russell_module = angular.module 'russell', []

russell_module.filter 'sec', () -> (t) ->

    s = Math.round(t / 1000)

    min = Math.floor(s / 60)
    sec = s - min*60

    if sec < 10
        "#{min}:0#{sec}"
    else
        "#{min}:#{sec}"

russell_module.factory 'address', () -> "#{window.location.hostname}"

