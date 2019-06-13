$(function(){
    function footerPosition(){
      $("footer").removeClass("fixed-footer");
      var contentHeight = document.body.scrollHeight,
          winHeight = window.innerHeight;
      if((contentHeight < winHeight)){
          $("footer").addClass("fixed-footer");
          $(".content").height(winHeight);
      } else {
          $("footer").addClass("fixed-footer");
          $(".content").height(contentHeight);
          }
      }
      footerPosition();
      $(window).resize(footerPosition);
});
