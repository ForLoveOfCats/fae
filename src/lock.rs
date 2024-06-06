use std::ops::{Deref, DerefMut};

#[derive(Debug)]
pub struct RwLock<T> {
	lock: parking_lot::RwLock<T>,
	#[cfg(feature = "duplicate-lock-checking")]
	locations: Locations,
}

#[cfg(feature = "duplicate-lock-checking")]
type Locations = std::sync::Arc<parking_lot::Mutex<std::collections::HashMap<std::thread::ThreadId, Location>>>;

#[cfg(feature = "duplicate-lock-checking")]
#[derive(Debug, PartialEq, Eq)]
enum AccessKind {
	Read,
	Write,
}

#[cfg(feature = "duplicate-lock-checking")]
impl std::fmt::Display for AccessKind {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			AccessKind::Read => f.write_str("read"),
			AccessKind::Write => f.write_str("write"),
		}
	}
}

#[cfg(feature = "duplicate-lock-checking")]
#[derive(Debug)]
struct Location {
	access_kind: AccessKind,
	backtrace: std::backtrace::Backtrace,
}

#[cfg(feature = "duplicate-lock-checking")]
#[track_caller]
fn report_duplicate_lock(location: &Location, current_access_kind: AccessKind) -> ! {
	println!("Duplicate lock access detected on a singular thread");
	println!();
	println!("detected here ({}):", current_access_kind);
	println!("{}", std::backtrace::Backtrace::force_capture());
	println!();
	println!("lock originally taken here ({}):", location.access_kind);
	println!("{}", location.backtrace);
	std::process::exit(-1);
}

impl<T> RwLock<T> {
	pub fn new(data: T) -> Self {
		RwLock {
			lock: parking_lot::RwLock::new(data),
			#[cfg(feature = "duplicate-lock-checking")]
			locations: std::sync::Arc::new(parking_lot::Mutex::new(std::collections::HashMap::new())),
		}
	}

	pub fn read<'a>(&'a self) -> ReadGuard<'a, T> {
		#[cfg(feature = "duplicate-lock-checking")]
		{
			let thread_id = std::thread::current().id();
			let mut locations = self.locations.lock();
			match locations.entry(thread_id) {
				std::collections::hash_map::Entry::Occupied(occupied) => {
					let location = occupied.get();
					report_duplicate_lock(location, AccessKind::Read);
				}

				std::collections::hash_map::Entry::Vacant(vacancy) => {
					vacancy.insert(Location {
						access_kind: AccessKind::Read,
						backtrace: std::backtrace::Backtrace::force_capture(),
					});
				}
			};
			drop(locations);

			ReadGuard { guard: self.lock.read(), locations: self.locations.clone() }
		}

		#[cfg(not(feature = "duplicate-lock-checking"))]
		ReadGuard { guard: self.lock.read() }
	}

	pub fn write<'a>(&'a self) -> WriteGuard<'a, T> {
		#[cfg(feature = "duplicate-lock-checking")]
		{
			let thread_id = std::thread::current().id();
			let mut locations = self.locations.lock();
			match locations.entry(thread_id) {
				std::collections::hash_map::Entry::Occupied(occupied) => {
					let location = occupied.get();
					report_duplicate_lock(location, AccessKind::Write);
				}

				std::collections::hash_map::Entry::Vacant(vacancy) => {
					vacancy.insert(Location {
						access_kind: AccessKind::Write,
						backtrace: std::backtrace::Backtrace::force_capture(),
					});
				}
			};
			drop(locations);

			WriteGuard { guard: self.lock.write(), locations: self.locations.clone() }
		}

		#[cfg(not(feature = "duplicate-lock-checking"))]
		WriteGuard { guard: self.lock.write() }
	}
}

pub struct ReadGuard<'a, T> {
	guard: parking_lot::RwLockReadGuard<'a, T>,
	#[cfg(feature = "duplicate-lock-checking")]
	locations: Locations,
}

impl<'a, T> Deref for ReadGuard<'a, T> {
	type Target = T;

	fn deref(&self) -> &T {
		self.guard.deref()
	}
}

#[cfg(feature = "duplicate-lock-checking")]
impl<'a, T> Drop for ReadGuard<'a, T> {
	fn drop(&mut self) {
		let thread_id = std::thread::current().id();
		let location = self.locations.lock().remove(&thread_id).unwrap();
		assert_eq!(location.access_kind, AccessKind::Read);
	}
}

pub struct WriteGuard<'a, T> {
	guard: parking_lot::RwLockWriteGuard<'a, T>,
	#[cfg(feature = "duplicate-lock-checking")]
	locations: Locations,
}

impl<'a, T> Deref for WriteGuard<'a, T> {
	type Target = T;

	fn deref(&self) -> &T {
		self.guard.deref()
	}
}

impl<'a, T> DerefMut for WriteGuard<'a, T> {
	fn deref_mut(&mut self) -> &mut T {
		self.guard.deref_mut()
	}
}

#[cfg(feature = "duplicate-lock-checking")]
impl<'a, T> Drop for WriteGuard<'a, T> {
	fn drop(&mut self) {
		let thread_id = std::thread::current().id();
		let location = self.locations.lock().remove(&thread_id).unwrap();
		assert_eq!(location.access_kind, AccessKind::Write);
	}
}
