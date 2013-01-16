russell_module.factory 'snake', ($http, $q, $timeout, make_url) ->

    snake = []
    statuses = (("" for i in [0..3]) for j in [0..3])

    in_snake = (x,y) -> status(x,y) == "selected"

    neighbour = ([x0,y0],[x1,y1]) ->
        Math.max(Math.abs(x0 - x1),Math.abs(y0 - y1)) <= 1

    status = (x,y) -> statuses[x][y]

    upd_status = (from,to,path) ->
        for [x,y] in path
            if statuses[x][y] == from
                statuses[x][y] = to

    status: status

    word: (lookup) -> (_.map snake, lookup).join('')

    erase: (user) ->
        for [x,y] in snake
            statuses[x][y] = "submitted"

        snake_copy = (x for x in snake)

        snake = []

        answer = $q.defer()

        handle_success = (res) ->
            console.log "Handling", res
            new_status = if res.correct then "correct" else "wrong"
            upd_status "submitted", new_status, snake_copy
            answer.resolve _.extend res, new_status: new_status
            clear_status = () ->
                upd_status new_status, "", snake_copy
            $timeout clear_status, 300

        post = $http.post (make_url "/submit/"),
            snake: snake_copy
            user: user

        post.success handle_success
        post.error -> $timeout (-> handle_success do ->
            correct = _.random 0, 1
            correct: 1 == correct
            score: correct * _.random 0, 1000
            words: correct * _.random 0, 100), 300

        answer.promise

    push: (coord) ->
        empty = _.isEmpty snake
        is_new = not (in_snake coord...)
        adjacent = empty or neighbour coord, _.last snake
        if empty or (is_new and adjacent)
            last_status = "selected"
            [x,y] = coord
            statuses[x][y] = "selected"
            snake.push coord


