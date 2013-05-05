russell_module.controller 'GridCtrl', ($scope, websocket, $q, $timeout) ->

    # Snake logic
    # The problem is that the snake logic takes care of the statuses
    # The division is meant that everything about the snake and the
    # statuses is in this little "class", and the outside takes
    # care of interacting with the webpage
    # This used to be a factory, but the scope needs to be applied
    # when the result of the websocket is returned. Maybe
    # this can be solved by submitting via the web socket in the
    # controller, but it is unclear what would be the simplest
    Snake = do ->

        snake = []

        $scope.statuses = []

        clear_statuses = ->
            $scope.statuses = (("" for i in [0..3]) for j in [0..3])

        clear_statuses()

        in_snake = (x,y) -> $scope.statuses[x][y] == "selected"

        neighbour = ([x0,y0],[x1,y1]) ->
            Math.max(Math.abs(x0 - x1),Math.abs(y0 - y1)) <= 1

        upd_status = (from,to,path) ->
            for [x,y] in path
                if $scope.statuses[x][y] == from
                    $scope.statuses[x][y] = to

        word: (lookup) -> (_.map snake, lookup).join('')

        clear: () ->
            snake = []
            clear_statuses()

        submit: () ->
            for [x,y] in snake
                $scope.statuses[x][y] = "submitted"

            snake_copy = (x for x in snake)

            snake = []

            answer = $q.defer()

            websocket.send
                Submit:
                    snake: snake_copy

            websocket.once "Response", (res) -> $scope.apply ->
                console.log "Handling", res
                new_status = if res.correct then "correct" else "wrong"
                console.log "New status: #{new_status}"
                upd_status "submitted", new_status, snake_copy
                answer.resolve _.extend res,
                    new_status: new_status
                $timeout (-> upd_status new_status, "", snake_copy), 300

            answer.promise

        push: (coord) ->
            empty = _.isEmpty snake
            is_new = not (in_snake coord...)
            adjacent = empty or neighbour coord, _.last snake
            if empty or (is_new and adjacent)
                last_status = "selected"
                [x,y] = coord
                $scope.statuses[x][y] = "selected"
                snake.push coord

    # Grid logic and controller
    $scope.status = (x,y) ->
        res = $scope.statuses[x][y]
        # console.log "Status #{x} #{y} = #{res}"
        res

    $scope.reset = () ->
        $scope.coord = [undefined,undefined]

        $scope.score = 0
        $scope.words = 0

        $scope.drawing = false
        $scope.last_status = ""
        $scope.last_word = ""

        Snake.clear()

        $scope.info = ""

    $scope.grid = [[]]

    $scope.scores = {}

    $scope.char_score = (char) -> $scope.scores[char]

    $scope.down = () ->
        $scope.drawing = true
        $scope.last_status = "selected"
        Snake.push($scope.coord)

    $scope.up = () ->
        $scope.drawing = false
        $scope.last_word = $scope.word()
        $scope.last_status = "submitted"
        Snake.submit().then (res) -> $scope.$apply ->
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
                Snake.push($scope.coord)

    $scope.lookup = (x,y) -> $scope.grid[y][x]

    $scope.word = () ->
        Snake.word((w) -> $scope.lookup w...) or $scope.last_word

    websocket.on "Grid", (data) -> $scope.$apply ->
        $scope.reset()
        $scope.grid = data.grid
        $scope.scores = _.object data.char_scores
        $scope.time = data.timeout
        $scope.$parent.play_mode = true

    websocket.on "ScoreBoard", (data) -> $scope.$apply ->
        $scope.time = data.timeout

