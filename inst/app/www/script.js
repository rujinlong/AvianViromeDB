// AvianViromeDB custom JavaScript

$(document).ready(function() {
  // Add responsive behavior to boxes
  $('.box').addClass('box-responsive');
  
  // Add hover effect to sidebar menu items
  $('.sidebar-menu > li').hover(
    function() { $(this).addClass('menu-item-hover'); },
    function() { $(this).removeClass('menu-item-hover'); }
  );
  
  // Initialize tooltips for any elements with data-toggle="tooltip"
  $('[data-toggle="tooltip"]').tooltip();
  
  // Smooth scroll to elements
  $(document).on('click', 'a.smooth-scroll', function(event) {
    event.preventDefault();
    
    $('html, body').animate({
      scrollTop: $($.attr(this, 'href')).offset().top - 70
    }, 500);
  });
});
