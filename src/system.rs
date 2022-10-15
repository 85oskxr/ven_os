mod allocator {
    pub mod heap {
        pub const SIZE: usize = 100 * 1024;
        pub const START: usize = 0x_4444_4444_0000;
    }
    use linked_list_allocator::*;
    use x86_64::{
        structures::paging::{
            mapper::MapToError, FrameAllocator, Mapper, Page, PageTableFlags, Size4KiB,
        },
        VirtAddr,
    };

    #[global_allocator] static ALLOCATOR: LockedHeap = LockedHeap::empty();

    pub fn init_heap(mapper: &mut impl Mapper<Size4KiB>, frame_allocator: &mut impl FrameAllocator<Size4KiB>) -> Result<(), MapToError<Size4KiB>> {
        let page_range = {
            let heap_start = VirtAddr::new(heap::START as u64);
            let heap_end = heap_start + heap::SIZE - 1u64;
            let heap_start_page = Page::containing_address(heap_start);
            let heap_end_page = Page::containing_address(heap_end);
            Page::range_inclusive(heap_start_page, heap_end_page)
        };
        for page in page_range {
            let frame = frame_allocator
                .allocate_frame()
                .ok_or(MapToError::FrameAllocationFailed)?;
            let flags = PageTableFlags::PRESENT | PageTableFlags::WRITABLE;
            unsafe { mapper.map_to(page, frame, flags, frame_allocator)?.flush() };
        }
        unsafe {
            ALLOCATOR.lock().init(heap::START, heap::SIZE);
        }
        Ok(())
    }
}
mod memory {

    use bootloader::bootinfo::{MemoryMap, MemoryRegionType};
    use x86_64::{
        structures::paging::{FrameAllocator, OffsetPageTable, PageTable, PhysFrame, Size4KiB},
        PhysAddr, VirtAddr,
    };

    pub unsafe fn init(physical_memory_offset: VirtAddr) -> OffsetPageTable<'static> {
        OffsetPageTable::new(active_level_4_table(physical_memory_offset), physical_memory_offset)
    }
    unsafe fn active_level_4_table(physical_memory_offset: VirtAddr) -> &'static mut PageTable {
        use x86_64::registers::control::Cr3;
        let (level_4_table_frame, _) = Cr3::read();
        let phys = level_4_table_frame.start_address();
        let virt = physical_memory_offset + phys.as_u64();
        let page_table_ptr: *mut PageTable = virt.as_mut_ptr();
        &mut *page_table_ptr
    }

    pub struct EmptyFrameAllocator;
    unsafe impl FrameAllocator<Size4KiB> for EmptyFrameAllocator {
        fn allocate_frame(&mut self) -> Option<PhysFrame<Size4KiB>> {
            None
        }
    }
    pub struct BootInfoFrameAllocator {
        memory_map: &'static MemoryMap,
        next: usize,
    }
    impl BootInfoFrameAllocator {
        pub unsafe fn init(memory_map: &'static MemoryMap) -> Self {
            BootInfoFrameAllocator {
                memory_map,
                next: 0,
            }
        }
        fn usable_frames(&self) -> impl Iterator<Item = PhysFrame> {
            let regions = self.memory_map.iter();
            let usable_regions = regions.filter(|r| r.region_type == MemoryRegionType::Usable);
            let addr_ranges = usable_regions.map(|r| r.range.start_addr()..r.range.end_addr());
            let frame_addresses = addr_ranges.flat_map(|r| r.step_by(4096));
            frame_addresses.map(|addr| PhysFrame::containing_address(PhysAddr::new(addr)))
        }
    }
    unsafe impl FrameAllocator<Size4KiB> for BootInfoFrameAllocator {
        fn allocate_frame(&mut self) -> Option<PhysFrame> {
            let frame = self.usable_frames().nth(self.next);
            self.next += 1;
            frame
        }
    }
}
pub mod console {
    pub const WIDTH: usize = 80;
    pub const HEIGHT: usize = 25;

    lazy_static::lazy_static! {
        static ref WRITER: spin::Mutex<Writer> = spin::Mutex::new(Writer {
            column_position: 0,
            foreground_color: Color::White,
            background_color: Color::Black,
            buffer: unsafe { &mut *(0xB8000 as *mut Buffer) }
        });
    }

    pub fn set_foreground_color(foreground_color: Color) -> Color {
        WRITER.lock().foreground_color = foreground_color;
        foreground_color
    }
    pub fn set_background_color(background_color: Color) -> Color {
        WRITER.lock().background_color = background_color;
        background_color
    }
    pub fn clear() {
        for _ in 0..WIDTH * HEIGHT {
            write!(" ");
        }
    }
    pub fn write_formatted_text(text: alloc::vec::Vec<super::language::Message>) {
        use super::language::Message;
        let theme: spin::MutexGuard<crate::system::theme::Theme> = crate::system::theme::ACTUAL_THEME.lock();
        for x in text {
            match x {
                Message::ColoredSuccess(string) => {
                    super::console::set_foreground_color(theme.colored_success);
                    super::console::write!("{}", string);
                },
                Message::ErrorPrefix(string) => {
                    super::console::set_foreground_color(theme.error_prefix);
                    super::console::write!("{}", string);
                },
                Message::ColoredError(string) => {
                    super::console::set_foreground_color(theme.colored_error);
                    super::console::write!("{}", string);
                },
                Message::SquareBracket(char) => {
                    super::console::set_foreground_color(theme.square_bracket);
                    super::console::write!("{}", char);
                },
                Message::InfoPrefix(string) => {
                    super::console::set_foreground_color(theme.info_prefix);
                    super::console::write!("{}", string);
                },
                Message::ClampBracket(char) => {
                    super::console::set_foreground_color(theme.clamp_bracket);
                    super::console::write!("{}", char);
                },
                Message::MathChars(char) => {
                    super::console::set_foreground_color(theme.math_chars);
                    super::console::write!("{}", char);
                },
                Message::Colored(string) => {
                    super::console::set_foreground_color(theme.colored);
                    super::console::write!("{}", string);
                },
                Message::Normal(string) => {
                    super::console::set_foreground_color(theme.normal);
                    super::console::write!("{}", string);
                },
                Message::Bracket(char) => {
                    super::console::set_foreground_color(theme.bracket);
                    super::console::write!("{}", char);
                },
                Message::EndChar(char) => {
                    super::console::set_foreground_color(theme.end_char);
                    super::console::write!("{}", char);
                },
                Message::EndLine => super::console::write!("\n"),
                Message::Space => super::console::write!(" "),
            }
        }
    }

    #[repr(u8)]
    #[allow(dead_code)]
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub enum Color {
        Black = 0,
        Blue = 1,
        Green = 2,
        Cyan = 3,
        Red = 4,
        Magenta = 5,
        Brown = 6,
        LightGray = 7,
        DarkGray = 8,
        LightBlue = 9,
        LightGreen = 10,
        LightCyan = 11,
        LightRed = 12,
        Pink = 13,
        Yellow = 14,
        White = 15,
    }
    impl core::fmt::Display for Color {
        fn fmt(&self, _: &mut core::fmt::Formatter<>) -> core::fmt::Result {
            Ok(())
        }
    }
    
    #[repr(transparent)]
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    struct ColorCode(u8);
    impl ColorCode {
        fn new(foreground_color: Color, background_color: Color) -> Self {
            Self((background_color as u8) << 4 | foreground_color as u8)
        }
    }
    
    #[repr(C)]
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    struct ScreenChar {
        character: u8,
        color_code: ColorCode,
    }
    
    #[repr(transparent)]
    struct Buffer {
        chars: [[volatile::Volatile<ScreenChar>; WIDTH]; HEIGHT]
    }
    
    struct Writer {
        column_position: usize,
        foreground_color: Color,
        background_color: Color,
        buffer: &'static mut Buffer,
    }
    impl Writer {
        pub fn write_byte(&mut self, byte: u8) {
            match byte {
                b'\n' => self.new_line(),
                byte => {
                    if self.column_position >= WIDTH {
                        self.new_line();
                    }
                    self.buffer.chars[HEIGHT - 1][self.column_position].write(ScreenChar {
                        character: byte,
                        color_code: ColorCode::new(self.foreground_color, self.background_color),
                    });
                    self.column_position += 1;
                }
            }
        }
        fn write_string(&mut self, string: &str) {
            for byte in string.bytes() {
                match byte {
                    0x20..=0x7E | b'\n' => self.write_byte(byte),
                    _ =>self.write_byte(0xFE),
                }
            }
        }
        fn new_line(&mut self) {
            for row in 1..HEIGHT {
                for col in 0..WIDTH {
                    self.buffer.chars[row - 1][col].write(self.buffer.chars[row][col].read());
                }
            }
            self.clear_row(HEIGHT - 1);
            self.column_position = 0;
        }
        fn clear_row(&mut self, row: usize) {
            for col in 0..WIDTH {
                self.buffer.chars[row][col].write(ScreenChar {
                    character: b' ',
                    color_code: ColorCode::new(self.foreground_color, self.background_color),
                });
            }
        }
    }
    impl core::fmt::Write for Writer {
        fn write_str(&mut self, s: &str) -> core::fmt::Result {
            self.write_string(s);
            Ok(())
        }
    }

    #[macro_export]
    macro_rules! console_write {
        ($($arg:tt)*) => ($crate::system::console::_print(format_args!($($arg)*)));
    }
    #[macro_export]
    macro_rules! console_write_line {
    () => ($crate::console_write!("\n"));
    ($($arg:tt)*) => ($crate::console_write!("{}\n", format_args!($($arg)*)));
    }

    pub use console_write as write;
    pub use console_write_line as write_line;

    #[doc(hidden)]
    pub fn _print(args: core::fmt::Arguments) {
        use core::fmt::Write;
        WRITER.lock().write_fmt(args).unwrap();
    }
}
pub mod environment {
    pub mod os {
        pub static NAME: &str = "VenOS";
    }
    pub fn type_of<T>(_: T) -> &'static str {
        core::any::type_name::<T>()
    }
}
pub mod language {
    use super::environment;
    use alloc::string::String;
    use core::panic::PanicInfo;
    use alloc::{format, string::ToString};

    lazy_static::lazy_static! {
        pub static ref ACTUAL_LANGUAGE: spin::Mutex<ActualLanguage> = spin::Mutex::new(ActualLanguage { value: Langs::Pl });
    }
    pub struct ActualLanguage {
        pub value: Langs
    }
    pub enum Langs {
        Pl,
        Eng,
    }
    pub enum Messages {
        BootSuccessfully,
        ErrorPanic(String, String, String),
        CpuExceptions(String, String, String, String, String)
    }
    pub enum Message {
        ColoredSuccess(String),
        ColoredError(String),
        ErrorPrefix(String),
        SquareBracket(char),
        InfoPrefix(String),
        ClampBracket(char),
        MathChars(char),
        Colored(String),
        Normal(String),
        Bracket(char),
        EndChar(char),
        EndLine,
        Space,
    }
    pub fn get_message(message: Messages) -> alloc::vec::Vec<Message> {
        match ACTUAL_LANGUAGE.lock().value {
            Langs::Eng => {
                match message {
                    Messages::BootSuccessfully =>  alloc::vec![Message::SquareBracket('['), Message::InfoPrefix(String::from("INFO")), Message::SquareBracket(']'), Message::Space, Message::MathChars('-'), Message::Normal(format!(" {} Booted ", environment::os::NAME)), Message::ColoredSuccess(String::from("Successfully")), Message::EndChar('!')],
                    Messages::ErrorPanic(error_message, location, col) => alloc::vec![Message::EndLine, Message::EndLine, Message::SquareBracket('['), Message::ErrorPrefix(String::from("PANIC")), Message::SquareBracket(']'), Message::Space, Message::MathChars('-'), Message::Space, Message::Normal(format!("{}", error_message)), Message::ColoredError(String::from(" | ")), Message::Normal(format!("{}", location)), Message::ColoredError(String::from(":")), Message::Normal(String::from(col)), Message::ColoredError(String::from("!"))],
                    Messages::CpuExceptions(code_segment, cpu_flags, instruction_pointer, stack_pointer, stack_segment) => alloc::vec![Message::EndLine, Message::EndLine, Message::SquareBracket('['), Message::ErrorPrefix(String::from("CPU EXCEPTION")), Message::SquareBracket(']'), Message::Space, Message::MathChars('-'), Message::Space, Message::ClampBracket('{'), Message::EndLine, Message::Normal(String::from("    code_segment")), Message::ColoredError(format!(": {},", code_segment)), Message::EndLine, Message::Normal(String::from("    cpu_flags")), Message::ColoredError(format!(": 0x{:x},", cpu_flags.parse::<u64>().unwrap())), Message::EndLine, Message::Normal(String::from("    instruction_pointer")), Message::ColoredError(format!(": {},", instruction_pointer)), Message::EndLine, Message::Normal(String::from("    stack_pointer")), Message::ColoredError(format!(": {},", stack_pointer)), Message::EndLine, Message::Normal(String::from("    stack_segment")), Message::ColoredError(format!(": {},", stack_segment)), Message::EndLine, Message::ClampBracket('}'), Message::ColoredError(String::from(";"))],
                }
            },
            Langs::Pl => {
                match message {
                    Messages::BootSuccessfully => alloc::vec![Message::SquareBracket('['), Message::InfoPrefix(String::from("INFO")), Message::SquareBracket(']'), Message::Space, Message::MathChars('-'), Message::Normal(format!(" {} Uruchomil sie ", environment::os::NAME)), Message::ColoredSuccess(String::from("poprawnie")), Message::EndChar('!')],
                    Messages::ErrorPanic(error_message, location, col) => alloc::vec![Message::EndLine, Message::EndLine, Message::SquareBracket('['), Message::ErrorPrefix(String::from("PANIC")), Message::SquareBracket(']'), Message::Space, Message::MathChars('-'), Message::Space, Message::Normal(format!("{}", error_message)), Message::ColoredError(String::from(" | ")), Message::Normal(format!("{}", location)), Message::ColoredError(String::from(":")), Message::Normal(String::from(col)), Message::ColoredError(String::from("!"))],
                    Messages::CpuExceptions(code_segment, cpu_flags, instruction_pointer, stack_pointer, stack_segment) => alloc::vec![Message::EndLine, Message::EndLine, Message::SquareBracket('['), Message::ErrorPrefix(String::from("CPU EXCEPTION")), Message::SquareBracket(']'), Message::Space, Message::MathChars('-'), Message::Space, Message::ClampBracket('{'), Message::EndLine, Message::Normal(String::from("    code_segment")), Message::ColoredError(format!(": {},", code_segment)), Message::EndLine, Message::Normal(String::from("    cpu_flags")), Message::ColoredError(format!(": 0x{:x},", cpu_flags.parse::<u64>().unwrap())), Message::EndLine, Message::Normal(String::from("    instruction_pointer")), Message::ColoredError(format!(": {},", instruction_pointer)), Message::EndLine, Message::Normal(String::from("    stack_pointer")), Message::ColoredError(format!(": {},", stack_pointer)), Message::EndLine, Message::Normal(String::from("    stack_segment")), Message::ColoredError(format!(": {},", stack_segment)), Message::EndLine, Message::ClampBracket('}'), Message::ColoredError(String::from(";"))],
                }
            },
        }
    }
    pub fn set_language(lang: Langs) {
        ACTUAL_LANGUAGE.lock().value = lang;
    }
}
pub mod theme {
    use super::console::Color;
    lazy_static::lazy_static! {
        pub static ref ACTUAL_THEME: spin::Mutex<Theme> = spin::Mutex::new(Theme::new(Themes::Default));
    }
    pub struct Theme {
        pub colored_success: Color,
        pub square_bracket: Color,
        pub colored_error: Color,
        pub clamp_bracket: Color,
        pub error_prefix: Color,
        pub info_prefix: Color,
        pub math_chars: Color,
        pub end_char: Color,
        pub bracket: Color,
        pub colored: Color,
        pub normal: Color,
    }
    impl Theme {
        pub fn new(theme: Themes) -> Self {
            match theme {
                Themes::Default => Self {
                    colored_success: Color::LightGreen,
                    colored_error: Color::LightRed,
                    square_bracket: Color::LightGray,
                    clamp_bracket: Color::LightGray,
                    error_prefix: Color::LightRed,
                    info_prefix: Color::LightGreen,
                    math_chars: Color::LightGray,
                    end_char: Color::White,
                    bracket: Color::White,
                    colored: Color::LightGreen,
                    normal: Color::White,
                },
            }
        }
    }
    pub enum Themes {
        Default,
    }
    pub fn set_theme(theme: Themes) {
        match theme {
            Themes::Default => {
                *ACTUAL_THEME.lock() = Theme::new(Themes::Default);
            }
        }
    }
}
pub mod cpu_exceptions {
    pub mod gdt {
        lazy_static::lazy_static! {
            static ref TSS: x86_64::structures::tss::TaskStateSegment = {
                let mut tss = x86_64::structures::tss::TaskStateSegment::new();
                tss.interrupt_stack_table[0u16 as usize] = {
                    const STACK_SIZE: usize = 4096 * 5;
                    static mut STACK: [u8; STACK_SIZE] = [0; STACK_SIZE];
                    let stack_start = x86_64::VirtAddr::from_ptr(unsafe { &STACK });
                    let stack_end = stack_start + STACK_SIZE;
                    stack_end
                };
                tss
            };
            static ref GDT: (x86_64::structures::gdt::GlobalDescriptorTable, Selectors) = {
                let mut gdt = x86_64::structures::gdt::GlobalDescriptorTable::new();
                let code_selector = gdt.add_entry(x86_64::structures::gdt::Descriptor::kernel_code_segment());
                let tss_selector = gdt.add_entry(x86_64::structures::gdt::Descriptor::tss_segment(&TSS));
                (
                    gdt,
                    Selectors {
                        code_selector,
                        tss_selector,
                    },
                )
            };
        }
        struct Selectors {
            code_selector: x86_64::structures::gdt::SegmentSelector,
            tss_selector: x86_64::structures::gdt::SegmentSelector,
        }
        pub fn init() {
            use x86_64::instructions::segmentation::{Segment, CS};
            use x86_64::instructions::tables::load_tss;

            GDT.0.load();
            unsafe {
                CS::set_reg(GDT.1.code_selector);
                load_tss(GDT.1.tss_selector);
            }
        }
        pub extern "x86-interrupt" fn double_fault_handler(stack_frame: x86_64::structures::idt::InterruptStackFrame, _error_code: u64) -> ! {
            crate::console::write_formatted_text(crate::language::get_message(crate::language::Messages::CpuExceptions(alloc::format!("{:?}", stack_frame.code_segment), alloc::format!("{}", stack_frame.cpu_flags), alloc::format!("{:?}", stack_frame.instruction_pointer), alloc::format!("{:?}", stack_frame.stack_pointer), alloc::format!("{}", stack_frame.stack_segment))));
            loop {
            }
        }
    }
    lazy_static::lazy_static! {
        static ref IDT: x86_64::structures::idt::InterruptDescriptorTable = {
            let mut idt = x86_64::structures::idt::InterruptDescriptorTable::new();
            idt.breakpoint.set_handler_fn(breakpoint_handler);
            unsafe {
                idt.double_fault.set_handler_fn(gdt::double_fault_handler)
                    .set_stack_index(0u16); // new
            }
            idt
        };
    }

    pub fn init_idt() {
        IDT.load();
    }

    extern "x86-interrupt" fn breakpoint_handler(stack_frame: x86_64::structures::idt::InterruptStackFrame) {
        super::console::write_formatted_text(super::language::get_message(super::language::Messages::CpuExceptions(alloc::format!("{:?}", stack_frame.code_segment), alloc::format!("{}", stack_frame.cpu_flags), alloc::format!("{:?}", stack_frame.instruction_pointer), alloc::format!("{:?}", stack_frame.stack_pointer), alloc::format!("{}", stack_frame.stack_segment))));
    }
}
pub fn init(boot_info: &'static bootloader::BootInfo, language: language::Langs, foreground_color: console::Color, background_color: console::Color) {
    let phys_mem_offset = x86_64::VirtAddr::new(boot_info.physical_memory_offset);
    let mut mapper = unsafe { memory::init(phys_mem_offset) };
    let mut frame_allocator = unsafe { memory::BootInfoFrameAllocator::init(&boot_info.memory_map) };
    allocator::init_heap(&mut mapper, &mut frame_allocator).expect("heap initialization failed");

    language::ACTUAL_LANGUAGE.lock().value = language;

    console::set_foreground_color(foreground_color);
    console::set_background_color(background_color);
    console::clear();

    cpu_exceptions::init_idt();
    cpu_exceptions::gdt::init();
}