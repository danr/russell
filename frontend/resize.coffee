
russell_module.factory 'resize', () ->

    recalc: () ->

        h = $(window).height()
        w = $(window).width()

        # The area is divided to six sections,
        # Top, the four grids, then the bottom.
        # A stripe is half the top (or the bottom)

        INNER = 0.75
        MARGIN = 1 - INNER
        BORDER = 0.015

        PANES = 1.5

        # Required height is w * PANES, if it is too much, make it less wide
        if h < w * PANES
            w = h / PANES

        stripe = Math.floor(h * 1/12)
        center = w
        leftover = Math.floor((h-(4*stripe+center))/2)

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
        row:
            width: Math.floor(w * 0.9)
            'font-size': Math.round(w * 0.7)

        font_small:
            'font-size': Math.round(stripe * 0.55)
        font_normal:
            'font-size': Math.round(stripe * 0.7)
        long_stripe:
            height: stripe + leftover
        stripe:
            height: stripe
        long_dbl_stripe:
            height: stripe*2 + leftover

        one_third:
            width: Math.floor(w * 0.3)

        small_indent:
            'margin-left': Math.floor(w*0.1)

        container:
            height: h
            width: w

