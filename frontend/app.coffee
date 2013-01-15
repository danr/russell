window.russel_module = angular.module('russel', [])

$(window).resize (e) ->


    h = $(window).height()
    w = $(window).width()
    console.log h, w

    # Divide into three panes,
    # top, center, bottom

    TOP_PANE = 0.1
    BOTTOM_PANE = 0.1

    INNER = 0.8
    MARGIN = 1 - INNER
    BORDER = 0.015

    PANES = 1 + TOP_PANE + BOTTOM_PANE
    # Required height is w * PANES, if it is too much, make it less wide
    if h < w * PANES
        w = h / PANES

    top    = Math.round(w * TOP_PANE)
    center = w
    bottom = Math.round(w * BOTTOM_PANE)

    $('div.container').css('width',Math.round center)

    # Sides of a tile
    side = center / 4 * 0.98
    inner = side * INNER
    margin = side * (MARGIN / 2)
    border = side * BORDER
    inner_r = Math.round inner
    border_r = Math.max 1, Math.round border
    margin_r = (Math.round margin) - border_r

    char_size = Math.round(inner * 0.84)
    score_size = Math.round(inner * 0.2)

    tiles = $('.tile')
        .css('width',inner_r)
        .css('height',inner_r)
        .css('margin',margin_r)
        .css('border-width',border_r)

    tiles.find('.char').css('font-size',char_size)
    tiles.find('.score').css('font-size',score_size)
    tiles.find('.shadow-score').css('font-size',score_size)

    $('div.word').css('font-size', top)

russel_module.controller 'TileCtrl', () ->
    $(window).trigger 'resize'

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
        x_str = tile.find('#x').text()
        y_str = tile.find('#y').text()
        if x_str and y_str
            x = Number(x_str)
            y = Number(y_str)
            # $scope.info = "#{char.text()} #{x} #{y} #{$scope.coord}"
            $scope.coord = [x,y]
            if $scope.drawing
                # console.log "pushing #{x} #{y}"
                $scope.push()

    in_snake = (x,y) -> _.some $scope.snake, (e) -> _.isEqual e, [x,y]

    $scope.push = ->
        if not in_snake $scope.coord...
            $scope.snake.push $scope.coord

    $scope.erase = -> $scope.snake = []

    $scope.status = (x,y) ->
        if in_snake x,y
            "selected"
        else
            ""

    $scope.lookup = (x,y) -> $scope.grid[y][x]

    $scope.word = () -> (_.map $scope.snake, (w) -> $scope.lookup w...).join('')

