(function () {
    
    function applyStyles (displayState, button) {
        var styles = {
            'original': {
                'delete'     : '',
                'add'        : 'hidden',
                'moved-from' : '',
                'moved-to'   : 'hidden'
            },
            
            'new': {
                'delete'     : 'hidden',
                'add'        : '',
                'moved-from' : 'hidden',
                'moved-to'   : ''
            },
            
            'diff': {
                'delete'     : 'diff',
                'add'        : 'diff',
                'moved-from' : 'diff',
                'moved-to'   : 'diff'
            },
        }[displayState];
        
        for (var style in styles) { 
            $('.' + style).removeClass().addClass(style).addClass(styles[style]);
        }
        
        $('.selected').toggleClass('selected');
        $(button).toggleClass('selected');
    }
    
    $(document).ready(function () {
        $('#show_diff').bind('click', function () { applyStyles('diff', this); }).click();
        $('#show_original').bind('click', function () { applyStyles('original', this); });
        $('#show_new').bind('click', function () { applyStyles('new', this); });
    });
    
})();