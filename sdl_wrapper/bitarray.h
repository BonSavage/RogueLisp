#ifndef BITARRAY_H_INCLUDED
#define BITARRAY_H_INCLUDED
#include <iterator>
#include <SDL2/sdl_stdinc.h>
using u8 = Uint8;

//It is because std::bitset is poorly implemented

class bit_iterator
{
public:
	class bit
	{
	public:
		bit(u8* b, u8 m) : byte_ptr_{ b },byte_{*b}, mask_{ m } {}
		operator bool(void){return (byte_ & mask_); }
		bit& operator++(void)
		{
            mask_ = mask_ >> 1;
            if (!mask_) {
                byte_=*(++byte_ptr_);
                mask_ = 0x80;
        }
    return *this;
    }
		void operator=(bool);
		friend bool operator!=(bit&,bit&);
	private:
		u8* byte_ptr_;
		u8 byte_;
		u8 mask_;
	};

	explicit bit_iterator(u8* init_byte,u8 init_mask = 0x80) : bit_{init_byte,init_mask} {}
	bit& operator*(void){return bit_;}
	bit_iterator operator++(void) {++bit_;return *this;}
	void operator=(bool val) {bit_ = val;}
	friend bool operator !=(bit_iterator,bit_iterator);
private:
	bit bit_;
};

template<size_t S>
class bit_array
{
public:
	bit_array(void)
	{
		clear();
	}

	void clear(void)
	{
		std::fill<u8*, u8>(bytes, bytes + size, 0);
	}

	bit_iterator begin(void)
	{
		return bit_iterator(bytes + 0);
	}

	bit_iterator end(void)
	{
		return bit_iterator(bytes + size - 1, end_mask);
	}

	bit_iterator::bit operator[](const size_t index)
	{
		u8* const byte = bytes + (index / 8);
		u8 mask = 0x80 >> (index % 8);
		return bit_iterator::bit(byte, mask);
	}
private:
	static const size_t size = S / 8 + 1;
	static const u8 end_mask = 0x80 >> (S % 8);
	u8 bytes[size];
};


#endif // BITARRAY_H_INCLUDED
