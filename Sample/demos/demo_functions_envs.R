# Test and Demo Closures, Functions and Environments

setup_env <- function(x) {
	local_var <- x # Parent environment for Child Function(s)
	
	save_and_add_one <- function(y) {
		assign("local_var", y, envir = parent.env(environment()))
		# write to the parent environment of this function.
		y + 1 # Implicit return
	}

	add_5 <- function() {
		local_var + 5 # Will look for variable going up the environments tree until found
	}

	add_origin_5 <- function() { # Overwriting "local_var" does not affect calling env's parameter values
		x + 5 # x is not lost, it is part of the setup_env() context
	}

	show_local_env_x <- function() {
		x <- 1 # This x is the first found
		x # when looking for the x object. It is NOT the parent function's parameter, in this case
	}

	# implicit return: The list will contain functions, which names we can change if needed:
	list(save_and_add_one = save_and_add_one,
		add_five = add_5,
		add_5_to_setup_call_param = add_origin_5,
		show_internal_function_env_x = show_local_env_x)
}

test_env <- setup_env(2) # so x will be two at first
test_env$add_five # So we expect 7
test_env$save_and_add_one(4) # Replace "local_var" with 4, and sum 1
test_env$add_five() # Now we have 4 in local_var, so that's a 9
test_env$add_5_to_setup_call_param() # We called setup_env with 2, so that should be a 7
test_env$show_internal_function_env_x() # Careful, we use a DIFFERENT x here, which we set to one.
test_env$add_5_to_setup_call_param() # But in fact the calling parameter x was untouched

