russell_module.controller 'GridCtrl', ($scope,$http,snake) ->

    $scope.user = ""

    $scope.logged_in = false

    $scope.login = () ->
        $http.get("http://localhost:3000/round").success (res) ->
            $scope.grid = res.round_grid
            $scope.scores = _.object res.round_char_scores
            $scope.logged_in = true

    $scope.coord = [undefined,undefined]

    $scope.score = 0
    $scope.words = 0

    $scope.drawing = false

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

    $scope.last_status = ""

    $scope.status = snake.status

    $scope.word = () ->
        snake.word((w) -> $scope.lookup w...) or $scope.last_word


