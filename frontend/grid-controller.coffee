russell_module.controller 'GridCtrl', ($scope, $http, snake, make_url) ->

    console.log $scope

    $scope.$watch 'logged_in', (lg) ->
        if lg
            $scope.get_grids()

    q = null

    $scope.get_grids = () ->
        console.log "Grid: getting grids"
        if q == null
            q = {}
            q = $http.get(make_url "/grid/")
            q.success (res) ->
                console.log "Grid: ", res
                $scope.reset()
                $scope.grid = res.grid_response
                $scope.scores = _.object res.round_char_scores
                $scope.time = res.grid_timeout
                $scope.$parent.play_mode = true
                $scope.$watch 'play_mode', () ->
                    if not $scope.play_mode
                        $scope.get_grids()
                q = null

    $scope.reset = () ->
        $scope.coord = [undefined,undefined]

        $scope.score = 0
        $scope.words = 0

        $scope.drawing = false
        $scope.last_status = ""
        $scope.last_word = ""

        snake.erase ""

        $scope.status = snake.status
        $scope.info = ""

    $scope.grid = [[]]

    $scope.scores = {}

    $scope.char_score = (char) -> $scope.scores[char]

    $scope.down = () ->
        $scope.drawing = true
        $scope.last_status = "selected"
        snake.push($scope.coord)

    $scope.up = () ->
        $scope.drawing = false
        $scope.last_word = $scope.word()
        $scope.last_status = "submitted"
        snake.erase($scope.user).then (res) ->
            $scope.last_status = res.new_status
            $scope.score = Math.max res.score, $scope.score
            $scope.words = Math.max res.words, $scope.words
            $scope.time = res.resp_timeout

    $scope.enter = ($event) ->
        if $event.originalEvent.type == "touchmove"
            $scope.drawing = true
        elem = $ document.elementFromPoint $event.pageX, $event.pageY
        tile = elem.closest '.tile'
        char = tile.find '.char'
        x_str = tile.find('#x').text()
        y_str = tile.find('#y').text()
        if x_str and y_str
            x = Number(x_str)
            y = Number(y_str)
            $scope.coord = [x,y]
            if $scope.drawing
                snake.push($scope.coord)

    $scope.lookup = (x,y) -> $scope.grid[y][x]

    $scope.word = () ->
        snake.word((w) -> $scope.lookup w...) or $scope.last_word


