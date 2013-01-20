russell_module.controller 'SummaryCtrl', ($scope, $http, snake, make_url) ->

    $scope.$watch 'logged_in', (lg) ->
        if lg
            $scope.get_summary()

    q = null

    $scope.get_summary = () ->
        console.log "Summary: getting summaries"
        if q == null
            q = {}
            q = $http.get(make_url "/summary/")
            q.success (res) ->
                console.log "Summary: ", res
                $scope.summary = _.object res.summary_scores
                $scope.$parent.play_mode = false
                $scope.$watch 'play_mode', () ->
                    if $scope.play_mode
                        $scope.get_summary()
                q = null

