russell_module.controller 'UserCtrl', ($scope, websocket, make_url, resize) ->

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
    resize.recalc (new_css) -> $scope.css = new_css

    # is this in AngularJS?
    $(window).resize -> resize.recalc (new_css) -> $scope.css = new_css

