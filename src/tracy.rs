#[cfg(feature = "tracy-profile")]
pub struct Main {}

#[cfg(feature = "tracy-profile")]
impl Main {
	pub fn new() -> Main {
		std::env::set_var("TRACY_NO_EXIT", "1");
		tracy_client::Client::start();
		Main {}
	}
}

#[cfg(feature = "tracy-profile")]
impl Drop for Main {
	fn drop(&mut self) {
		unsafe { tracy_client::sys::___tracy_shutdown_profiler() }
	}
}

#[cfg(feature = "tracy-profile-zones")]
pub struct Zone {
	_span: tracy_client::Span,
}

#[cfg(feature = "tracy-profile-zones")]
impl Zone {
	pub fn new(span: tracy_client::Span) -> Zone {
		Zone { _span: span }
	}
}

#[cfg(feature = "tracy-profile-zones")]
macro_rules! zone {
	($name: literal) => {
		$crate::tracy::Zone::new(tracy_client::span!($name, 0))
	};
}

#[cfg(not(feature = "tracy-profile-zones"))]
macro_rules! zone {
	($name: literal) => {
		()
	};
}

#[cfg(feature = "tracy-profile")]
macro_rules! set_thread_name {
	($name: literal) => {
		tracy_client::set_thread_name!($name)
	};
}

#[cfg(not(feature = "tracy-profile"))]
macro_rules! set_thread_name {
	($name: literal) => {};
}
