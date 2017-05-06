use misc::{StrSpan, Location};
use std::ops::{Range, RangeFrom, RangeTo, RangeFull};
use nom::{FindSubstring, Slice};

pub trait GetLocation<'a, R> {
    fn get_loc(&self, range: R) -> Location<'a>;
}

impl<'a, 'b> GetLocation<'a, Range<&'b str>> for StrSpan<'a> {
    fn get_loc(&self, range: Range<&'b str>) -> Location<'a> {
        let start = self.find_substring(range.start).expect("start not found");
        let end = start + self.slice(start..).find_substring(range.end)
            .expect("end not found");
        Location {
            start: self.slice(start..start),
            end: self.slice(end..end)
        }
    }
}

impl<'a, 'b> GetLocation<'a, RangeFrom<&'b str>> for StrSpan<'a> {
    fn get_loc(&self, range: RangeFrom<&'b str>) -> Location<'a> {
        let start = self.find_substring(range.start).expect("start not found");
        Location {
            start: self.slice(start..start),
            end: self.slice(self.fragment.len()..)
        }
    }
}

impl<'a, 'b> GetLocation<'a, RangeTo<&'b str>> for StrSpan<'a> {
    fn get_loc(&self, range: RangeTo<&'b str>) -> Location<'a> {
        let end = self.find_substring(range.end).expect("end not found");
        Location {
            start: self.slice(0..0),
            end: self.slice(end..end)
        }
    }
}

impl<'a> GetLocation<'a, RangeFull> for StrSpan<'a> {
    fn get_loc(&self, _: RangeFull) -> Location<'a> {
        Location {
            start: self.slice(0..0),
            end: self.slice(self.fragment.len()..)
        }
    }
}
