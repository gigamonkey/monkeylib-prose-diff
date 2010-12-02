(function () {

    function setup () {
        $('#show_original').bind('click', showOriginal);
        $('#show_new').bind('click', showNew);
        $('#show_diff').bind('click', showDiff);
        showDiff();
    }

    function showOriginal () {
        $('.delete').removeClass().addClass('delete');
        $('.add').removeClass().addClass('add hidden');
        $('.moved-from').removeClass().addClass('moved-from');
        $('.moved-to').removeClass().addClass('moved-to hidden');
    }

    function showNew () {
        $('.delete').removeClass().addClass('delete hidden');
        $('.add').removeClass().addClass('add');
        $('.moved-from').removeClass().addClass('moved-from hidden');
        $('.moved-to').removeClass().addClass('moved-to');
    }

    function showDiff () {
        $('.delete').removeClass().addClass('delete delete-display');
        $('.add').removeClass().addClass('add add-display');
        $('.moved-from').removeClass().addClass('moved-from moved-from-display');
        $('.moved-to').removeClass().addClass('moved-to moved-to-display');
    }

    $(document).ready(setup);
    
})();