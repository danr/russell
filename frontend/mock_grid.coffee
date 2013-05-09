
window.mock_grid = ($timeout) ->

    time = 900

    scores =
        you: [0,0]
        Agda: [0,0]
        Ada: [0,0]

    update_player = (player,value) ->
        [words,score] = scores[player]
        scores[player] = [words+1,score+value]

    random_update = (player) ->
        correct = _.random 0,1
        score = correct * _.random 0,10
        if correct
            update_player player, score
        correct: correct
        score: score

    # scoreboard callback
    sbcb = ->

    send_sb = ->
        random_update "Agda"
        random_update "Ada"
        sbcb
            timeout: time-- * 1000
            scores: ([name,w,s] for name, [w,s] of scores)
        $timeout send_sb, 1000

    send_sb()

    connect: ->

    send : ->

    once : (cmd, cb) ->
        if cmd == "Response"
            cb random_update "you"

    on : (cmd, cb) ->
        if cmd == "ScoreBoard"
            sbcb = (x) -> cb(x)

        if cmd == "Connected"
            cb
                username: "dan"

        if cmd == "Grid"
            cb
                timeout: time * 1000
                grid:
                    [ "AOEU"
                    , "IDHT"
                    , "NSPY"
                    , "FGCR"
                    ]
                char_scores:
                    [ ['A',1]
                    , ['B',4]
                    , ['C',8]
                    , ['D',1]
                    , ['E',1]
                    , ['F',4]
                    , ['G',2]
                    , ['H',3]
                    , ['I',1]
                    , ['J',8]
                    , ['K',3]
                    , ['L',1]
                    , ['M',3]
                    , ['N',1]
                    , ['O',2]
                    , ['P',3]
                    , ['Q',10]
                    , ['R',1]
                    , ['S',1]
                    , ['T',1]
                    , ['U',3]
                    , ['V',4]
                    , ['X',10]
                    , ['Y',8]
                    , ['Z',10]
                    , ['Å',4]
                    , ['Ä',4]
                    , ['Ö',4]
                    ]


