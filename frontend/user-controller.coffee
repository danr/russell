russell_module.controller 'UserCtrl', ($scope, make_url) ->

    $scope.user = ""

    $scope.logged_in = false

    $scope.play_mode = false

    $scope.login = () -> if $scope.user != ""
        console.log "Logged in!"
        $scope.logged_in = true

