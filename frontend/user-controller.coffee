russell_module.controller 'UserCtrl', ($scope, websocket, resize) ->

    $scope.user = ""

    $scope.logged_in = false

    $scope.play_mode = false

    $scope.login = () -> if $scope.user != ""
        console.log "Logging in..."
        websocket.send
            Connect:
                username: $scope.user

    websocket.on "Connected", (data) -> $scope.$apply ->
        $scope.logged_in = true

    $scope.css = {}

    $scope.css = resize.recalc()

    # is this in AngularJS?
    $(window).resize -> $scope.$apply ->
        $scope.css = resize.recalc()

    $scope.join = (objs...) ->
        s = {}
        for o in objs
            for k,v of o
                s[k] = v
        s

    console.log $scope.join (apa: 1), (cepa: 2), (epa: 3)
