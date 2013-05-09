window.russell_module = angular.module 'russell', []

russell_module.factory 'address', () -> "#{window.location.hostname}"

set_ws = (m) -> russell_module.factory 'websocket', m

q = (s) -> window.location.search.contains s
if q "scoreboard" then set_ws mock_scoreboard
else if q "grid" then set_ws mock_grid
else set_ws websocket_factory

russell_module.run (websocket, address, $log) ->
    $log.info "Connecting to websocket"
    websocket.connect("ws://#{address}:8000")

russell_module.filter 'sec', () -> (t) ->

    s = Math.round(t / 1000)

    min = Math.floor(s / 60)
    sec = s - min*60

    if sec < 10
        "#{min}:0#{sec}"
    else
        "#{min}:#{sec}"

