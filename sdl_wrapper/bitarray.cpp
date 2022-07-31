#include "bitarray.h"

/* bit_iterator::bit& bit_iterator::bit::operator++(void)
{
    mask_ = mask_ >> 1;
    if (!mask_) {
        byte_=*(++byte_ptr_);
        mask_ = 0x80;
    }
    return *this;
} */

bool operator!=(bit_iterator::bit& b1,bit_iterator::bit& b2)
{
    return !(b1.byte_ptr_ == b2.byte_ptr_ && b1.mask_ == b2.mask_);
}

bool operator !=(bit_iterator i1,bit_iterator i2)
{
	return (i1.bit_ != i2.bit_); //Gives undefined behavior when compare within ==
}

void bit_iterator::bit::operator=(bool b)
{
	if (b)
		(*byte_ptr_) |= mask_;
	else if (!b)
		(*byte_ptr_) &= ~mask_;
}
