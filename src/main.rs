#![no_std]
#![no_main]
#![feature(panic_info_message)]
#![feature(abi_x86_interrupt)]
#![feature(default_alloc_error_handler)]

mod system;
extern crate alloc;

// #region
use alloc::{
    vec, vec::Vec,
    string::{String, ToString}, borrow::ToOwned,
};
use system::{
    console::{Color, self},
    environment::{type_of},
    language::{Langs, Messages, Message, get_message, self}

};
// #endregion

bootloader::entry_point!(main);

fn main(boot_info: &'static bootloader::BootInfo) -> ! {
    system::init(boot_info, Langs::Eng, Color::White, Color::Black);
    console::write_formatted_text(language::get_message(Messages::BootSuccessfully));
    loop {

    }
}

#[panic_handler]
fn panic_handler(info: &core::panic::PanicInfo) -> ! {
    console::write_formatted_text(language::get_message(Messages::ErrorPanic(info.message().unwrap().to_string(), info.location().unwrap().file().to_string(), info.location().unwrap().line().to_string())));
    loop {

    }
}