//ccavenue-validate-billing-name. Have not set maximum limit of 60 characters. Identical to validate-alpha.
Validation.addAllThese([
['ccavenue-validate-billing-name', 'Please use letters only (a-z or A-Z) in this field.', function (v) {
                return Validation.get('IsEmpty').test(v) ||  /^[a-zA-Z ]+$/.test(v)
            }],
//ccavenue-billing-address. Copy of Alpha num with spaces with modifications
['ccavenue-billing-address', 'Please use only the following: letters (a-z or A-Z), numbers, slashes (/), hashes(#), commas(,), brackets(), hyphen(-) or spaces.', function(v) {
                    return Validation.get('IsEmpty').test(v) || /^[-a-zA-Z0-9/#,.() ]+$/.test(v)
            }],
//ccavenue-billing-phone. Only numbers
['ccavenue-billing-phone', 'Please use only numbers', function(v) {
                    return Validation.get('IsEmpty').test(v) || /^[0-9]+$/.test(v)
            }],
// zipcode validation in billing section, only in Indian website level
['validate-billing-indian-zipcode', 'Please enter a valid zip code', function(v) {
                if (document.getElementById("billing:country_id")){
                	var country_id = document.getElementById("billing:country_id").value;
                	if(country_id == 'IN'){
                		if (document.getElementById("advice-validate-billing-indian-zipcode-billing:postcode"))
                            document.getElementById("advice-validate-billing-indian-zipcode-billing:postcode").innerHTML = "Please use a 6 digit pincode without spaces";
                        return Validation.get('IsEmpty').test(v) || /^\d{6}$/.test(v) && !(/[0]{6}/.test(v));
                	}else{ // validation copied from internation zip code
                        if (document.getElementById("advice-validate-billing-indian-zipcode-billing:postcode"))
                            document.getElementById("advice-validate-billing-indian-zipcode-billing:postcode").innerHTML = "Please enter a valid zip code";
                		return Validation.get('IsEmpty').test(v) || /(^[A-z0-9]{2,10}([\s]{0,1}|[\-]{0,1})[A-z0-9]{2,10}$)/.test(v);
                	}
                }
            }],
// zipcode validation in shipping section, only in Indian website level
['validate-shipping-indian-zipcode', 'Please use a 6 digit pincode without spaces', function(v) {
                    return Validation.get('IsEmpty').test(v) || /^\d{6}$/.test(v) && !(/[0]{6}/.test(v))
            }],
// zipcode validation in account page new address section, only in Indian website level
['accountpage-zipcode-validation', 'Please enter a valid zip code', function(v) {
                if (document.getElementById("country")){
                    var country_id = document.getElementById("country").value;
                    if(country_id == 'IN'){
                        if (document.getElementById("advice-accountpage-zipcode-validation-zip"))
                            document.getElementById("advice-accountpage-zipcode-validation-zip").innerHTML = "Please use a 6 digit pincode without spaces";
                        return Validation.get('IsEmpty').test(v) || /^\d{6}$/.test(v) && !(/[0]{6}/.test(v));
                    }else{ // validation copied from internation zip code
                        if (document.getElementById("advice-accountpage-zipcode-validation-zip"))
                            document.getElementById("advice-accountpage-zipcode-validation-zip").innerHTML = "Please enter a valid zip code";
                        return Validation.get('IsEmpty').test(v) || /(^[A-z0-9]{2,10}([\s]{0,1}|[\-]{0,1})[A-z0-9]{2,10}$)/.test(v);
                    }
                }
            }]
]);                          