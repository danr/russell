russell_module.controller 'GridCtrl', ($scope, websocket, snake, make_url) ->

    websocket.on "Grid", (data) -> $scope.$apply ->
        $scope.reset()
        $scope.grid = data.grid
        $scope.scores = _.object data.char_scores
        $scope.time = data.timeout
        $scope.$parent.play_mode = true

    websocket.on "ScoreBoard", (data) -> $scope.$apply ->
        $scope.time = data.timeout

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
        snake.erase().then (res) -> $scope.$apply ->
            $scope.last_status = res.new_status
            if res.correct
                $scope.score += res.score
                $scope.words++

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

