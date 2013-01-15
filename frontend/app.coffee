window.russel_module = angular.module('russel', [])

$(window).resize (e) ->


    h = $(window).height()
    w = $(window).width()
    console.log h, w

    # Divide into three panes,
    # top, center, bottom

    TOP_PANE = 0.1
    BOTTOM_PANE = 0.1

    INNER = 0.75
    MARGIN = 1 - INNER
    BORDER = 0.015

    PANES = 1 + TOP_PANE + BOTTOM_PANE

    # Required height is w * PANES, if it is too much, make it less wide
    if h < w * PANES
        w = h / PANES

    top    = Math.floor(w * TOP_PANE)
    center = w
    bottom = Math.floor(w * BOTTOM_PANE)

    space = Math.floor ((h - top - center - bottom) / 2)

    $('#top-space,#bottom-space').css('height',space)

    $('div.container').css('width',Math.floor center)

    # Sides of a tile
    side = center / 4
    inner = side * INNER
    margin = side * (MARGIN / 2)
    border = side * BORDER
    inner_r = Math.floor inner
    border_r = Math.max 1, Math.floor border
    margin_r = (Math.floor margin) - border_r


    char_size = Math.floor(inner * 0.84)
    score_size = Math.floor(inner * 0.2)

    tiles = $('.tile')
        .css('width',inner_r)
        .css('height',inner_r)
        .css('margin',margin_r)
        .css('border-width',border_r)

    tiles.find('.char').css('font-size',char_size)
    tiles.find('.score').css('font-size',score_size)
    tiles.find('.shadow-score').css('font-size',score_size)

    $('#top').css('height',top).css('font-size',top)
    $('#bottom').css('height',bottom).css('font-size',bottom)

russel_module.controller 'TileCtrl', () ->
    $(window).trigger 'resize'

russel_module.controller 'GridCtrl', ($scope,$timeout) ->

    $scope.coord = [undefined,undefined]

    $scope.drawing = false

    $scope.info = ""

    $scope.snake = []

    $scope.grid =
        [ "ACKS"
          "RLIA"
          "ÄOTR"
          "NHIE"
        ]
        #        [ ["E","N","K","N"]
        #          ["T","R","A","G"]
        #          ["A","P","Å","A"]
        #          ["L","S","V","K"]
        #        ]

    $scope.score = (char) -> $scope.scores[char]

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

    in_snake = (x,y) -> $scope.status(x,y) == "selected"

    neighbour = ([x0,y0],[x1,y1]) ->
        Math.max(Math.abs(x0 - x1),Math.abs(y0 - y1)) <= 1

    $scope.push = ->
        empty = _.isEmpty $scope.snake
        is_new = not (in_snake $scope.coord...)
        adjacent = empty or neighbour $scope.coord, _.last $scope.snake
        if empty or (is_new and adjacent)
            $scope.last_status = "selected"
            [x,y] = $scope.coord
            $scope.statuses[x][y] = "selected"
            $scope.snake.push $scope.coord

    $scope.last_word = ""
    $scope.last_status = ""

    $scope.erase = ->
        $scope.last_status = _.shuffle(["wrong","correct"])[0]
        $scope.last_word = $scope.word()
        for [x,y] in $scope.snake
            $scope.statuses[x][y] = $scope.last_status
        $scope.snake = []
        clear_status = () ->
            $scope.last_word = ""
            $scope.last_status = ""
            for x in [0..3]
                for y in [0..3]
                    if $scope.statuses[x][y] != "selected"
                        $scope.statuses[x][y] = ""
        $timeout clear_status, 300

    $scope.statuses = (("" for i in [0..3]) for j in [0..3])

    $scope.status = (x,y) -> $scope.statuses[x][y]

    $scope.lookup = (x,y) -> $scope.grid[y][x]

    $scope.word = () ->
        snake_word = (_.map $scope.snake, (w) -> $scope.lookup w...).join('')
        snake_word or $scope.last_word

