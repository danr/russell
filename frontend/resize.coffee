$(window).resize (e) ->

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

    $('#top-space,#bottom-space').css('height',space)

    $('.oneThird').css('width',Math.floor((w * 0.9)/3))

    $('div.container')
        .css('width',Math.floor center)
        .css('height',Math.floor h)

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

    $('#top').css('height',top).css('font-size',Math.round(top * 0.9))
    $('#bottom').css('height',bottom).css('font-size',Math.round(bottom * 0.9))

$(window).trigger 'resize'
