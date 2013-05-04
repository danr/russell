window.chat_module = angular.module('chat',[])

chat_module.factory 'log', () -> (x) -> console.log(x)

chat_module.factory 'address', () -> "192.168.1.66"

chat_module.factory 'websocket', ($q, log) ->
    # to run, inject thus:
    # myModule.run (websocket) ->
    # websocket.connect("ws://localhost:8000")

    # websocket.on "myCommand", (data) ->
    # c.log "data received!", data

    # websocket.send "my outgoing message"

    def = $q.defer()
    command_map = {}
    ws = null

    connect : (url) -> unless ws
        ws = new WebSocket(url)

        ws.onmessage = (evt) ->
            log "message #{evt.data}"
            # log "message #{JSON.parse evt.data} #{evt.data} #{evt.data.length}", evt.data, evt
            [[command,data]] = _.pairs JSON.parse evt.data
            for listener in command_map[command]
                listener(data)

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
            command_map[command_str].push callback

    off : (command_str, callback) ->
        command_map[command_str]?.splice command_map.indexOf(command_str), 1

chat_module.run (websocket, address, log) ->
    log "Sending to websocket"
    websocket.connect("ws://#{address}:8000")

chat_module.controller 'UserCtrl', ($scope, websocket, address, log) ->

    $scope.logged_in = false

    $scope.user = ""

    $scope.login = () ->
        websocket.send
            Connect:
                username: $scope.username

    websocket.on "Connected", (data) -> $scope.$apply ->
        log data
        $scope.logged_in = true
        $scope.user = data.username

    websocket.on "InvalidUsername", (data) ->
        log data
        $scope.logged_in = false

    $scope.send = () ->
        if $scope.logged_in
            websocket.send
                Send:
                    message: $scope.message
            $scope.message = ""

chat_module.controller 'MessageCtrl', ($scope, websocket, log) ->
    $scope.msgs = []
    websocket.on "Broadcast", (data) -> $scope.$apply ->
        $scope.msgs.push data

    websocket.on "Userlist", (data) -> $scope.$apply ->
        $scope.msgs.push
            username: ""
            message: data.usernames.toString()

