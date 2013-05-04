russell_module.controller 'SummaryCtrl', ($scope, websocket, snake, make_url) ->

    websocket.on "FinalScores", (data) -> $scope.$apply ->
        # ??
        # $scope.summary = data.final_scores
        console.log data
        $scope.summary = {}
        data.final_scores.sort (s1,s2) -> s2-s1
        $scope.summary = for [name,words] in data.final_scores
            name: name
            score: _.reduce (w[1] for w in words), ((a,b) -> a+b), 0
            words: words.length
            best: for [w,s] in _.take words.sort((w1,w2) -> w2[1] - w1[1]),4
                word: w
                score: s
        $scope.$parent.play_mode = false
        $scope.time = data.timeout

