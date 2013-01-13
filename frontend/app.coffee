window.russel_module = angular.module('russel', [])

russel_module.controller 'GridCtrl', ($scope) ->
    $scope.coord = [undefined,undefined]

    $scope.drawing = false

    $scope.info = ""

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

    $scope.up = () ->
        $scope.drawing = false
        $scope.erase()

    $scope.enter = ($event) ->
        # $scope.info = "enter #{$event.screenY} #{$event.screenX} #{$event.originalEvent.type}"
        if $event.originalEvent.type == "touchmove"
            # if we're on a touchpad, there is no down message
            $scope.drawing = true
        # console.log $event
        elem = $(document.elementFromPoint($event.pageX, $event.pageY))
        tile = elem.closest('.tile')
        char = tile.find('.char')
        x = tile.find('.x').text()
        y = tile.find('.y').text()
        # $scope.info = "#{char.text()} #{x} #{y} #{$event.pageX} #{$event.pageY}"
        $scope.coord = [x,y]
        if $scope.drawing and x and y
            # console.log "pushing #{x} #{y}"
            $scope.push()

    in_snake = (x,y) -> _.some $scope.snake, (e) -> _.isEqual e, [x,y]

    $scope.push = ->
        if not in_snake $scope.coord...
            $scope.snake.push $scope.coord

    $scope.erase = -> $scope.snake = []

    $scope.selected = in_snake

    $scope.lookup = (x,y) -> $scope.grid[y][x]

    $scope.word = () -> (_.map $scope.snake, (w) -> $scope.lookup w...).join('')



