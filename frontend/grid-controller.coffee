russell_module.controller 'GridCtrl', ($scope,$timeout,snake) ->

    $scope.coord = [undefined,undefined]

    $scope.score = 0
    $scope.words = 0

    $scope.drawing = false

    $scope.info = ""

    $scope.grid =
        [ "ACKS"
          "RLIA"
          "ÄOTR"
          "NHIE"
        ]

    $scope.scores =
        'A': 1
        'B': 4
        'C': 8
        'D': 1
        'E': 1
        'F': 4
        'G': 2
        'H': 3
        'I': 1
        'J': 8
        'K': 3
        'L': 1
        'M': 3
        'N': 1
        'O': 2
        'P': 3
        'Q': 10
        'R': 1
        'S': 1
        'T': 1
        'U': 3
        'V': 4
        'X': 10
        'Y': 8
        'Z': 10
        'Å': 4
        'Ä': 4
        'Ö': 4

    $scope.char_score = (char) -> $scope.scores[char]

    $scope.down = () ->
        $scope.drawing = true
        $scope.last_status = "selected"
        snake.push($scope.coord)

    $scope.up = () ->
        $scope.drawing = false
        $scope.last_word = $scope.word()
        $scope.last_status = "submitted"
        snake.erase().then (res) ->
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


