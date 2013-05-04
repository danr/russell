window.russell_module = angular.module 'russell', []

russell_module.factory 'make_url', () -> (s) -> "http://#{window.location.host}" + s

russell_module.filter 'sec', () -> (t) ->

    s = Math.round(t / 1000)

    min = Math.floor(s / 60)
    sec = s - min*60

    if sec < 10
        "#{min}:0#{sec}"
    else
        "#{min}:#{sec}"

russell_module.factory 'log', () -> (x) -> console.log(x)

russell_module.factory 'address', () -> "192.168.1.66"

russell_module.factory 'websocket', ($q, log) ->

    def = $q.defer()
    command_map = {}
    ws = null

    connect : (url) -> unless ws
        ws = new WebSocket(url)

        ws.onmessage = (evt) ->
            log "message #{evt.data}"
            [[command,data]] = _.pairs JSON.parse evt.data
            listeners = command_map[command]
            if listeners
                new_listeners = []
                for [beh,listener] in command_map[command]
                    listener(data)
                    if beh == 'multiple'
                        new_listeners.push [beh,listener]
                command_map[command] = new_listeners

        ws.onopen = (evt) ->
            log "onopen"
            def.resolve(evt)

        ws.onerror = (evt) ->
            log "onerror"
            def.reject(evt)

    send: (msg) ->
        log "send"
        def.promise.then () ->
            log "send #{msg} #{JSON.stringify msg}"
            ws.send JSON.stringify msg

    close : () ->
        log "closed!?"
        def.promise.then () ->
            log "closed!"
            ws.close()

    on : (command_str, callback) ->
        log "on #{command_str}"
        unless command_str in command_map
            command_map[command_str] = []
            command_map[command_str].push ['multiple', callback]

    once : (command_str, callback) ->
        log "on #{command_str}"
        unless command_str in command_map
            command_map[command_str] = []
            command_map[command_str].push ['once', callback]

russell_module.run (websocket, address, log) ->
    log "Sending to websocket"
    websocket.connect("ws://#{address}:8000")

