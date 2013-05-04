
window.russell_module.factory 'resize', () ->

    recalc: (set_css) ->

        h = $(window).height()
        w = $(window).width()

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

        # $('#top-space,#bottom-space').css('height',space)

        # $('.oneThird').css('width',Math.floor((w * 0.9)/3))

        # $('div.container')
        #     .css('width',Math.floor center)
        #     .css('height',Math.floor h)

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

        set_css
            tile:
                width: inner_r
                height: inner_r
                margin: margin_r
                'border-width': border_r
            char:
                'font-size': char_size
            score:
                'font-size': score_size
            shadow_score:
                'font-size': score_size
            top:
                height: top
                'font-size': Math.round(top * 0.9)
            bottom:
                height: bottom
                'font-size': Math.round(bottom * 0.9)
            top_space:
                height: space
            bottom_space:
                height: space
            row:
                width: Math.floor(w * 0.9)
                'font-size': Math.round(bottom * 0.8)
            row_ident:
                'margin-left': Math.floor(w * 0.1)
                'font-size': Math.round(bottom * 0.5)
            one_third:
                width: Math.floor((w * 0.9) / 3)
            container:
                width: Math.floor center
                height: Math.floor h

