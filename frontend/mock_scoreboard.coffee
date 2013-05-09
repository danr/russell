
window.mock_scoreboard = ->

    connect: ->

    send : ->

    on : (command_str, cb) ->
        if command_str == "Connected"
            cb
                username: "dan"
        if command_str == "FinalScores"
            cb
                timeout: 100
                final_scores:
                    [["Agda",[["GREN",10],["APA",13],["STUVNING",32],["BANAN",20],["RABIES",12],["GRODLÅR",44]]]
                    ,["Ada",[["SA",2],["ER",2]]]
                    ,["Agder",[["III",3]]]
                    ]


