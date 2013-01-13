window.russel_module = angular.module('russel', [])

russel_module.controller 'GridCtrl', ($scope) ->
    $scope.coord = [undefined,undefined]

    $scope.drawing = false

    $scope.snake = []

    $scope.grid =
        [ ["E","N","K","N"]
          ["T","R","A","G"]
          ["A","P","Å","A"]
          ["L","S","V","K"]
        ]

    $scope.score = (char) -> 1

    debug = () ->
        #    console.log "Drawing: ", $scope.drawing
        #    console.log "Coord: ", $scope.coord
        #    console.log "Snake: ", $scope.snake
        #    console.log "Word: ", $scope.word()


    $scope.down = () ->
        $scope.drawing = true
        $scope.push()
        debug()

    $scope.up = () ->
        $scope.drawing = false
        $scope.erase()
        debug()

    $scope.enter = (x,y) ->
        $scope.coord = [x,y]
        if $scope.drawing
            $scope.push()
        debug()

    in_snake = (x,y) -> _.some $scope.snake, (e) -> _.isEqual e, [x,y]

    $scope.push = ->
        if not in_snake $scope.coord...
            $scope.snake.push $scope.coord

    $scope.erase = ->
        $scope.snake = []

    $scope.selected = in_snake

    $scope.lookup = (x,y) -> $scope.grid[y][x]

    $scope.word = () -> (_.map $scope.snake, (w) -> $scope.lookup w...).join('')


