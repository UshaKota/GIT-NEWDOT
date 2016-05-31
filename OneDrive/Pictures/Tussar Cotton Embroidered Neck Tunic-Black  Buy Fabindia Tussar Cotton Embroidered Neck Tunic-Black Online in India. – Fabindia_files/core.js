/* ---------------------- */
/* Fabindia Core JavaScript
/* By: Redstart
/* www.redstart.in
/* ---------------------- */

// jQuery no conflict
var $j = jQuery.noConflict();

/* DROP DOWN MENU FUNCTIONS START */
var timeout    = 500;
var closetimer = 0;
var ddmenuitem = 0;

function jsddm_open() {
	jsddm_canceltimer();
	jsddm_close();
	ddmenuitem = $j(this).find('ul').css('visibility', 'visible');
}

function jsddm_close() {
	if(ddmenuitem)
		ddmenuitem.css('visibility', 'hidden');
}

function jsddm_timer() {
	closetimer = window.setTimeout(jsddm_close, timeout);
}

function jsddm_canceltimer() {
	if(closetimer) {
		window.clearTimeout(closetimer);
		closetimer = null;
	}
}
/* DROP DOWN MENU FUNCTIONS END */

// Main document ready function
$j(document).ready(function(){
	/* SELECT STORE START */
	if($j('#choose-store').length != 0 && $j.cookie('my_fab_store') != 'ind') {
		$j.blockUI({
			message: $j('#choose-store').html(),
			css: { border:'0px', padding:20, width:'552px', height:'282px', textAlign:'left', cursor: 'default', top: ($j(window).height() - 340) / 2 + 'px', left: ($j(window).width() - 565) / 2 + 'px' },
			overlayCSS: { cursor: 'default' },
			onBlock: function(){
				$j('.blockUI.blockMsg .choose.india').click(function(){
					$j.cookie('my_fab_store', 'ind', { path: '/', expires: 1 });
					$j.unblockUI();
					
					return false;
				});
				$j('.blockUI.blockMsg .choose.international').click(function(){
					$j.cookie('my_fab_store', 'intl', { path: '/', expires: 1 });
					location.href = $j('#store-change-url').attr('value');
					
					return false;
				});
			}
		});
	}
	/* SELECT STORE END */
	
	/* DROP DOWN MENU START */
	$j('#store-nav > li').bind('mouseover', jsddm_open);
	$j('#store-nav > li').bind('mouseout',  jsddm_timer);
	/* DROP DOWN MENU END */
	
	/* MAIN SEARCH START */
	$j('#store-search .search-box').focus(function(){
		if($j(this).attr('value') == 'Search')
			$j(this).attr('value', '');
	});
	$j('#store-search .search-box').blur(function(){
		if($j(this).attr('value') == '')
			$j(this).attr('value', 'Search');
	});
	$j('#store-search-form').submit(function(){
		if($j('#store-search .search-box').attr('value') == 'Search' || $j('#store-search .search-box').attr('value').length < 4) {
			alert('Please enter a search term of at least 3 characters');
			$j('#store-search .search-box').focus();
			return false;
		}
		return true;
	});
	$j('#store-search .search-box').blur();
	/* MAIN SEARCH END */
	
	/* DELIVERY DESTINATION START */
	$j('#user-nav .delivery-destination').click(function(){
		//$j('#user-nav .to').css('visibility', 'visible');
		$j('#user-nav .to').fadeIn();
		return false;
	});
	$j(document).click(function(){
		$j('#user-nav .to').fadeOut();
	});
	
	// Shipping International
	$j('#user-nav .to').click(function(){
		$j.cookie('my_fab_store', 'intl', { path: '/' });
		location.href = $j('#store-change-url').attr('value');
		
		return false;
	});
	/* DELIVERY DESTINATION END */
	
	/* GIFT COUPONS START */
	if($j('#coupon-container #coupon_code').length != 0) {
		if($j('#coupon-container #coupon_code').attr('value') == '')
			$j('#coupon-container #coupon_code').attr('value', 'Have a discount coupon?');
		
		$j('#coupon-container #coupon_code').focus(function(){
			if($j(this).attr('value') == 'Have a discount coupon?')
				$j(this).attr('value', '');
		});
		$j('#coupon-container #coupon_code').blur(function(){
			if($j(this).attr('value') == '')
				$j(this).attr('value', 'Have a discount coupon?');
		});
		
		$j('#coupon-container .coupon-get').fancybox({
			'width': 400,
			'height': 260,
			'autoDimensions': false
		});
	}
	/* GIFT COUPONS END */
	
	/* FOOTER NEWSLETTER START */
	$j('#footer .newsletter-box').focus(function(){
		if($j(this).attr('value') == 'Email address')
			$j(this).attr('value', '');
	});
	$j('#footer .newsletter-box').blur(function(){
		if($j(this).attr('value') == '')
			$j(this).attr('value', 'Email address');
	});
	$j('#footer .newsletter-box').blur();
	/* FOOTER NEWSLETTER END */
	
	/* PRODUCT LIGHTBOX START */
	$j("a.lightbox").fancybox();
	$j("a.lightboxgroup").fancybox({
		'transitionIn'	:	'elastic',
		'transitionOut'	:	'elastic',
		'speedIn'		:	600, 
		'speedOut'		:	200,
		'titlePosition' : 'over'
	});
	/* PRODUCT LIGHTBOX END */
	
	/* PRODUCT PAGE TABS START */
	$j('#tab-container .tab-nav a').click(function(){
		var theTab = $j(this).attr('href').replace('#', '');
		
		// Check if tab already open
		if($j('#tab-container .tab.' + theTab).is('.open'))
			return false;
		
		// Close all tabs
		$j('#tab-container .tab').removeClass('open');
		$j('#tab-container .tab-nav a').removeClass('selected');
		
		// Open selected tab
		$j('#tab-container .tab.' + theTab).addClass('open');
		$j(this).addClass('selected');
		
		return false;
	});
	/* PRODUCT PAGE TABS END */
	
	/* CHECKOUT PAGE START */
	// Remove all countries except India from shipping
	if($j('.checkoutcontainer #shipping_address .input-country select').length != 0) {
		$j('.checkoutcontainer #shipping_address .input-country select').children().remove().end().append('<option value="IN">India</option>');
	}
	/* CHECKOUT PAGE END */
	
	/* HOME NEWSLETTER START */
	$j('#home-newsletter .email, #home-newsletter .go, .newsletter-form .newsletter-box').click(function(){
		$j.fancybox({
			'type': 'iframe',
			'width': 700,
			'height': 400,
			'href': 'http://www.fabindia.com/mc-signup.html'
		});
		
		return false;
	});
	/* HOME NEWSLETTER END */
});