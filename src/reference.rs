use std::boxed::Box;
use std::hash::Hash;
use std::ops::Deref;
use std::ptr::NonNull;

#[cfg(feature = "measure-lock-contention")]
use std::alloc;
#[cfg(feature = "measure-lock-contention")]
use std::mem::{align_of, size_of};
#[cfg(feature = "measure-lock-contention")]
use std::sync::atomic::{AtomicUsize, Ordering};

#[derive(Debug)]
pub struct Ref<T> {
	pointer: NonNull<RefAllocated<T>>,
}

unsafe impl<T: Sync + Send> Send for Ref<T> {}
unsafe impl<T: Sync + Send> Sync for Ref<T> {}

struct RefAllocated<T> {
	#[cfg(feature = "measure-lock-contention")]
	reference_count: AtomicUsize,
	data: T,
}

impl<T> Clone for Ref<T> {
	#[inline]
	fn clone(&self) -> Self {
		#[cfg(feature = "measure-lock-contention")]
		self.allocated().reference_count.fetch_add(1, Ordering::Relaxed);

		Self { pointer: self.pointer }
	}
}

impl<T: PartialEq> PartialEq for Ref<T> {
	#[inline]
	fn eq(&self, other: &Ref<T>) -> bool {
		self.allocated().data.eq(&other.allocated().data)
	}

	#[inline]
	fn ne(&self, other: &Ref<T>) -> bool {
		self.allocated().data.ne(&other.allocated().data)
	}
}

impl<T: Eq> Eq for Ref<T> {}

impl<T: Hash> Hash for Ref<T> {
	fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
		self.allocated().data.hash(state)
	}
}

impl<T> Deref for Ref<T> {
	type Target = T;

	#[inline]
	fn deref(&self) -> &T {
		&self.allocated().data
	}
}

impl<T> AsRef<T> for Ref<T> {
	fn as_ref(&self) -> &T {
		&**self
	}
}

impl<T> std::borrow::Borrow<T> for Ref<T> {
	fn borrow(&self) -> &T {
		&**self
	}
}

impl<T> Ref<T> {
	#[inline]
	pub fn new(data: T) -> Self {
		let boxed = Box::new(RefAllocated {
			#[cfg(feature = "measure-lock-contention")]
			reference_count: AtomicUsize::new(1),
			data,
		});
		Ref { pointer: Box::leak(boxed).into() }
	}

	#[inline]
	fn allocated(&self) -> &RefAllocated<T> {
		unsafe { self.pointer.as_ref() }
	}
}

#[cfg(feature = "measure-lock-contention")]
impl<T> Drop for Ref<T> {
	fn drop(&mut self) {
		if self.allocated().reference_count.fetch_sub(1, Ordering::Release) != 1 {
			return;
		}

		std::sync::atomic::fence(Ordering::Acquire);

		unsafe {
			std::ptr::drop_in_place::<T>(&mut self.pointer.as_mut().data);
			let layout = alloc::Layout::new::<RefAllocated<T>>();
			alloc::dealloc(self.pointer.as_ptr() as _, layout);
		}
	}
}

#[derive(Debug)]
pub struct SliceRef<T> {
	#[cfg(feature = "measure-lock-contention")]
	reference_count_pointer: NonNull<AtomicUsize>,
	data: NonNull<[T]>,
}

unsafe impl<T: Sync + Send> Send for SliceRef<T> {}
unsafe impl<T: Sync + Send> Sync for SliceRef<T> {}

impl<T> Clone for SliceRef<T> {
	#[inline]
	fn clone(&self) -> Self {
		#[cfg(feature = "measure-lock-contention")]
		unsafe { self.reference_count_pointer.as_ref() }.fetch_add(1, Ordering::Relaxed);

		Self {
			#[cfg(feature = "measure-lock-contention")]
			reference_count_pointer: self.reference_count_pointer,
			data: self.data,
		}
	}
}

impl<T: PartialEq> PartialEq for SliceRef<T> {
	#[inline]
	fn eq(&self, other: &SliceRef<T>) -> bool {
		unsafe { self.data.as_ref().eq(other.data.as_ref()) }
	}

	#[inline]
	fn ne(&self, other: &SliceRef<T>) -> bool {
		unsafe { self.data.as_ref().ne(other.data.as_ref()) }
	}
}

impl<T: Eq> Eq for SliceRef<T> {}

impl<T: Hash> Hash for SliceRef<T> {
	fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
		self.data.hash(state)
	}
}

impl<T> Deref for SliceRef<T> {
	type Target = [T];

	#[inline]
	fn deref(&self) -> &[T] {
		unsafe { self.data.as_ref() }
	}
}

impl<T> AsRef<[T]> for SliceRef<T> {
	fn as_ref(&self) -> &[T] {
		&**self
	}
}

impl<T> std::borrow::Borrow<[T]> for SliceRef<T> {
	fn borrow(&self) -> &[T] {
		&**self
	}
}

#[cfg(feature = "measure-lock-contention")]
impl<T> Drop for SliceRef<T> {
	fn drop(&mut self) {
		if unsafe { self.reference_count_pointer.as_ref() }.fetch_sub(1, Ordering::Release) != 1 {
			return;
		}

		std::sync::atomic::fence(Ordering::Acquire);

		if self.data.len() == 0 {
			return;
		}

		unsafe {
			let len = self.data.len();
			std::ptr::drop_in_place(self.data.as_mut());

			let size = size_of::<T>() * len;
			let alignment = align_of::<T>();
			let layout = alloc::Layout::from_size_align_unchecked(size, alignment);
			alloc::dealloc(self.data.as_ptr() as _, layout);
		}
	}
}

#[cfg(feature = "measure-lock-contention")]
impl<T> From<Vec<T>> for SliceRef<T> {
	fn from(value: Vec<T>) -> Self {
		let boxed = Box::new(AtomicUsize::new(1));
		SliceRef {
			reference_count_pointer: Box::leak(boxed).into(),
			data: value.leak().into(),
		}
	}
}

impl<T> SliceRef<T> {
	pub fn new_empty() -> Self {
		#[cfg(feature = "measure-lock-contention")]
		let boxed = Box::new(AtomicUsize::new(1));

		let data = unsafe {
			let pointer = NonNull::<T>::dangling().as_ptr();
			std::slice::from_raw_parts_mut(pointer, 0)
		};

		Self {
			#[cfg(feature = "measure-lock-contention")]
			reference_count_pointer: Box::leak(boxed).into(),
			data: data.into(),
		}
	}
}

#[cfg(not(feature = "measure-lock-contention"))]
impl<T> From<Vec<T>> for SliceRef<T> {
	fn from(value: Vec<T>) -> Self {
		SliceRef { data: value.leak().into() }
	}
}
