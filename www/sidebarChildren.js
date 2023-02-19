// для каждого пункта меню назначается обработчик события click

$(function() {
   $('.menu-open > .active').parentsUntil('.sidebar', 'li').children('a:first-child').addClass('has-selected-child');
   $('.menu-open > li').on('click', function() {
      let $me = $(this);
      let $menu = $me.parents('.main-sidebar');
      $menu.find('.has-selected-child').removeClass('has-selected-child');
      $me.parentsUntil('.sidebar', 'li').children('a:first-child').addClass('has-selected-child');
   })
})