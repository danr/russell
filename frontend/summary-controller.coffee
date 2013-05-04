russell_module.controller 'SummaryCtrl', ($scope, websocket, snake, make_url) ->

    websocket.on "FinalScores", (data) -> $scope.$apply ->
        # ??
        # $scope.summary = data.final_scores
        $scope.summary = {}
        for [name,score,words] in data.final_scores
            $scope.summary[name] =
                score: score
                words: words.length
        $scope.$parent.play_mode = false
        $scope.time = data.timeout

