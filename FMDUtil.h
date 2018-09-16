// Fill out your copyright notice in the Description page of Project Settings.

#pragma once

#include <string>
#include <wchar.h>

/**
 * 
 */
namespace FMD
{
	constexpr const unsigned int HIGH_SURROGATE_START_CODEPOINT = 0xD800;
	constexpr const unsigned int HIGH_SURROGATE_END_CODEPOINT = 0xDBFF;
	constexpr const unsigned int LOW_SURROGATE_START_CODEPOINT = 0xDC00;
	constexpr const unsigned int LOW_SURROGATE_END_CODEPOINT = 0xDFFF;
	constexpr const unsigned int ENCODED_SURROGATE_START_CODEPOINT = 0x10000;
	constexpr const unsigned int ENCODED_SURROGATE_END_CODEPOINT = 0x10FFFF;

#define FMD_DEFAULT_STRING_CONVERSION_SIZE 128u
#define FMD_UNICODE_BOGUS_CHAR_CODEPOINT '?'
#define FMD_MAX_int32		((int)	0x7fffffff)
#define FMD_MAX_uint32		((unsigned int)	0xffffffff)

	/** Is the provided Codepoint within the range of the high-surrogates? */
	static inline bool IsHighSurrogate(const unsigned int Codepoint)
	{
		return Codepoint >= HIGH_SURROGATE_START_CODEPOINT && Codepoint <= HIGH_SURROGATE_END_CODEPOINT;
	}

	/** Is the provided Codepoint within the range of the low-surrogates? */
	static inline bool IsLowSurrogate(const unsigned int Codepoint)
	{
		return Codepoint >= LOW_SURROGATE_START_CODEPOINT && Codepoint <= LOW_SURROGATE_END_CODEPOINT;
	}

	/** Is the provided Codepoint outside of the range of the basic multilingual plane, but within the valid range of UTF8/16? */
	static inline bool IsEncodedSurrogate(const unsigned int Codepoint)
	{
		return Codepoint >= ENCODED_SURROGATE_START_CODEPOINT && Codepoint <= ENCODED_SURROGATE_END_CODEPOINT;
	}

	struct FMDCountingOutputIterator
	{
		FMDCountingOutputIterator()
			: Counter(0)
		{
		}

		const FMDCountingOutputIterator& operator* () const { return *this; }
		const FMDCountingOutputIterator& operator++() { ++Counter; return *this; }
		const FMDCountingOutputIterator& operator++(int) { ++Counter; return *this; }
		const FMDCountingOutputIterator& operator+=(const int Amount) { Counter += Amount; return *this; }

		template <typename T>
		T operator=(T Val) const
		{
			return Val;
		}

		friend int operator-(FMDCountingOutputIterator Lhs, FMDCountingOutputIterator Rhs)
		{
			return Lhs.Counter - Rhs.Counter;
		}

		int GetCount() const { return Counter; }

	private:
		int Counter;
	};

	namespace UTF8_TO_UNICODE
	{

		static unsigned int CodepointFromUtf8(const char*& SourceString, const unsigned int SourceLengthRemaining)
		{
			//checkSlow(SourceLengthRemaining > 0)

			const char* OctetPtr = SourceString;

			unsigned int Codepoint = 0;
			unsigned int Octet = (unsigned int)((unsigned char)*SourceString);
			unsigned int Octet2, Octet3, Octet4;

			if (Octet < 128)  // one octet char: 0 to 127
			{
				++SourceString;  // skip to next possible start of codepoint.
				return Octet;
			}
			else if (Octet < 192)  // bad (starts with 10xxxxxx).
			{
				// Apparently each of these is supposed to be flagged as a bogus
				//  char, instead of just resyncing to the next valid codepoint.
				++SourceString;  // skip to next possible start of codepoint.
				return FMD_UNICODE_BOGUS_CHAR_CODEPOINT;
			}
			else if (Octet < 224)  // two octets
			{
				// Ensure our string has enough characters to read from
				if (SourceLengthRemaining < 2)
				{
					// Skip to end and write out a single char (we always have room for at least 1 char)
					SourceString += SourceLengthRemaining;
					return FMD_UNICODE_BOGUS_CHAR_CODEPOINT;
				}

				Octet -= (128 + 64);
				Octet2 = (unsigned int)((unsigned char) *(++OctetPtr));
				if ((Octet2 & (128 + 64)) != 128)  // Format isn't 10xxxxxx?
				{
					++SourceString;  // Sequence was not valid UTF-8. Skip the first byte and continue.
					return FMD_UNICODE_BOGUS_CHAR_CODEPOINT;
				}

				Codepoint = ((Octet << 6) | (Octet2 - 128));
				if ((Codepoint >= 0x80) && (Codepoint <= 0x7FF))
				{
					SourceString += 2;  // skip to next possible start of codepoint.
					return Codepoint;
				}
			}
			else if (Octet < 240)  // three octets
			{
				// Ensure our string has enough characters to read from
				if (SourceLengthRemaining < 3)
				{
					// Skip to end and write out a single char (we always have room for at least 1 char)
					SourceString += SourceLengthRemaining;
					return FMD_UNICODE_BOGUS_CHAR_CODEPOINT;
				}

				Octet -= (128 + 64 + 32);
				Octet2 = (unsigned int)((unsigned char) *(++OctetPtr));
				if ((Octet2 & (128 + 64)) != 128)  // Format isn't 10xxxxxx?
				{
					++SourceString;  // Sequence was not valid UTF-8. Skip the first byte and continue.
					return FMD_UNICODE_BOGUS_CHAR_CODEPOINT;
				}

				Octet3 = (unsigned int)((unsigned char) *(++OctetPtr));
				if ((Octet3 & (128 + 64)) != 128)  // Format isn't 10xxxxxx?
				{
					++SourceString;  // Sequence was not valid UTF-8. Skip the first byte and continue.
					return FMD_UNICODE_BOGUS_CHAR_CODEPOINT;
				}

				Codepoint = (((Octet << 12)) | ((Octet2 - 128) << 6) | ((Octet3 - 128)));

				// UTF-8 characters cannot be in the UTF-16 surrogates range
				if (FMD::IsHighSurrogate(Codepoint) || FMD::IsLowSurrogate(Codepoint))
				{
					++SourceString;  // Sequence was not valid UTF-8. Skip the first byte and continue.
					return FMD_UNICODE_BOGUS_CHAR_CODEPOINT;
				}

				// 0xFFFE and 0xFFFF are illegal, too, so we check them at the edge.
				if ((Codepoint >= 0x800) && (Codepoint <= 0xFFFD))
				{
					SourceString += 3;  // skip to next possible start of codepoint.
					return Codepoint;
				}
			}
			else if (Octet < 248)  // four octets
			{
				// Ensure our string has enough characters to read from
				if (SourceLengthRemaining < 4)
				{
					// Skip to end and write out a single char (we always have room for at least 1 char)
					SourceString += SourceLengthRemaining;
					return FMD_UNICODE_BOGUS_CHAR_CODEPOINT;
				}

				Octet -= (128 + 64 + 32 + 16);
				Octet2 = (unsigned int)((unsigned char) *(++OctetPtr));
				if ((Octet2 & (128 + 64)) != 128)  // Format isn't 10xxxxxx?
				{
					++SourceString;  // Sequence was not valid UTF-8. Skip the first byte and continue.
					return FMD_UNICODE_BOGUS_CHAR_CODEPOINT;
				}

				Octet3 = (unsigned int)((unsigned char) *(++OctetPtr));
				if ((Octet3 & (128 + 64)) != 128)  // Format isn't 10xxxxxx?
				{
					++SourceString;  // Sequence was not valid UTF-8. Skip the first byte and continue.
					return FMD_UNICODE_BOGUS_CHAR_CODEPOINT;
				}

				Octet4 = (unsigned int)((unsigned char) *(++OctetPtr));
				if ((Octet4 & (128 + 64)) != 128)  // Format isn't 10xxxxxx?
				{
					++SourceString;  // Sequence was not valid UTF-8. Skip the first byte and continue.
					return FMD_UNICODE_BOGUS_CHAR_CODEPOINT;
				}

				Codepoint = (((Octet << 18)) | ((Octet2 - 128) << 12) |
					((Octet3 - 128) << 6) | ((Octet4 - 128)));
				if ((Codepoint >= 0x10000) && (Codepoint <= 0x10FFFF))
				{
					SourceString += 4;  // skip to next possible start of codepoint.
					return Codepoint;
				}
			}
			// Five and six octet sequences became illegal in rfc3629.
			//  We throw the codepoint away, but parse them to make sure we move
			//  ahead the right number of bytes and don't overflow the buffer.
			else if (Octet < 252)  // five octets
			{
				// Ensure our string has enough characters to read from
				if (SourceLengthRemaining < 5)
				{
					// Skip to end and write out a single char (we always have room for at least 1 char)
					SourceString += SourceLengthRemaining;
					return FMD_UNICODE_BOGUS_CHAR_CODEPOINT;
				}

				Octet = (unsigned int)((unsigned char) *(++OctetPtr));
				if ((Octet & (128 + 64)) != 128)  // Format isn't 10xxxxxx?
				{
					++SourceString;  // Sequence was not valid UTF-8. Skip the first byte and continue.
					return FMD_UNICODE_BOGUS_CHAR_CODEPOINT;
				}

				Octet = (unsigned int)((unsigned char) *(++OctetPtr));
				if ((Octet & (128 + 64)) != 128)  // Format isn't 10xxxxxx?
				{
					++SourceString;  // Sequence was not valid UTF-8. Skip the first byte and continue.
					return FMD_UNICODE_BOGUS_CHAR_CODEPOINT;
				}

				Octet = (unsigned int)((unsigned char) *(++OctetPtr));
				if ((Octet & (128 + 64)) != 128)  // Format isn't 10xxxxxx?
				{
					++SourceString;  // Sequence was not valid UTF-8. Skip the first byte and continue.
					return FMD_UNICODE_BOGUS_CHAR_CODEPOINT;
				}

				Octet = (unsigned int)((unsigned char) *(++OctetPtr));
				if ((Octet & (128 + 64)) != 128)  // Format isn't 10xxxxxx?
				{
					++SourceString;  // Sequence was not valid UTF-8. Skip the first byte and continue.
					return FMD_UNICODE_BOGUS_CHAR_CODEPOINT;
				}

				SourceString += 5;  // skip to next possible start of codepoint.
				return FMD_UNICODE_BOGUS_CHAR_CODEPOINT;
			}

			else  // six octets
			{
				// Ensure our string has enough characters to read from
				if (SourceLengthRemaining < 6)
				{
					// Skip to end and write out a single char (we always have room for at least 1 char)
					SourceString += SourceLengthRemaining;
					return FMD_UNICODE_BOGUS_CHAR_CODEPOINT;
				}

				Octet = (unsigned int)((unsigned char) *(++OctetPtr));
				if ((Octet & (128 + 64)) != 128)  // Format isn't 10xxxxxx?
				{
					++SourceString;  // Sequence was not valid UTF-8. Skip the first byte and continue.
					return FMD_UNICODE_BOGUS_CHAR_CODEPOINT;
				}

				Octet = (unsigned int)((unsigned char) *(++OctetPtr));
				if ((Octet & (128 + 64)) != 128)  // Format isn't 10xxxxxx?
				{
					++SourceString;  // Sequence was not valid UTF-8. Skip the first byte and continue.
					return FMD_UNICODE_BOGUS_CHAR_CODEPOINT;
				}

				Octet = (unsigned int)((unsigned char) *(++OctetPtr));
				if ((Octet & (128 + 64)) != 128)  // Format isn't 10xxxxxx?
				{
					++SourceString;  // Sequence was not valid UTF-8. Skip the first byte and continue.
					return FMD_UNICODE_BOGUS_CHAR_CODEPOINT;
				}

				Octet = (unsigned int)((unsigned char) *(++OctetPtr));
				if ((Octet & (128 + 64)) != 128)  // Format isn't 10xxxxxx?
				{
					++SourceString;  // Sequence was not valid UTF-8. Skip the first byte and continue.
					return FMD_UNICODE_BOGUS_CHAR_CODEPOINT;
				}

				Octet = (unsigned int)((unsigned char) *(++OctetPtr));
				if ((Octet & (128 + 64)) != 128)  // Format isn't 10xxxxxx?
				{
					++SourceString;  // Sequence was not valid UTF-8. Skip the first byte and continue.
					return FMD_UNICODE_BOGUS_CHAR_CODEPOINT;
				}

				SourceString += 6;  // skip to next possible start of codepoint.
				return FMD_UNICODE_BOGUS_CHAR_CODEPOINT;
			}

			++SourceString;  // Sequence was not valid UTF-8. Skip the first byte and continue.
			return FMD_UNICODE_BOGUS_CHAR_CODEPOINT;  // catch everything else.
		}

		template <typename DestBufferType>
		static void Convert_Impl(DestBufferType& ConvertedBuffer, int DestLen, const char* Source, const int SourceLen)
		{
			const char* SourceEnd = Source + SourceLen;
			while (Source < SourceEnd && DestLen > 0)
			{
				// Read our codepoint, advancing the source pointer
				unsigned int Codepoint = CodepointFromUtf8(Source, Source - SourceEnd);

				// We want to write out two chars
				if (FMD::IsEncodedSurrogate(Codepoint))
				{
					// We need two characters to write the surrogate pair
					if (DestLen >= 2)
					{
						Codepoint -= 0x10000;
						const wchar_t HighSurrogate = (Codepoint >> 10) + FMD::HIGH_SURROGATE_START_CODEPOINT;
						const wchar_t LowSurrogate = (Codepoint & 0x3FF) + FMD::LOW_SURROGATE_START_CODEPOINT;
						*(ConvertedBuffer++) = HighSurrogate;
						*(ConvertedBuffer++) = LowSurrogate;
						DestLen -= 2;
						continue;
					}

					// If we don't have space, write a bogus character instead (we should have space for it)
					Codepoint = FMD_UNICODE_BOGUS_CHAR_CODEPOINT;
				}
				else if (Codepoint > FMD::ENCODED_SURROGATE_END_CODEPOINT)
				{
					// Ignore values higher than the supplementary plane range
					Codepoint = FMD_UNICODE_BOGUS_CHAR_CODEPOINT;
				}

				*(ConvertedBuffer++) = Codepoint;
				--DestLen;
			}
		}

		static inline void Convert(wchar_t* Dest, const int DestLen, const char* Source, const int SourceLen)
		{
			Convert_Impl(Dest, DestLen, Source, SourceLen);
		}

		static inline int ConvertedLength(const char* Source, int SourceLen)
		{
			FMD::FMDCountingOutputIterator Dest;
			Convert_Impl(Dest, FMD_MAX_int32, Source, SourceLen);

			return Dest.GetCount();
		}

		const std::wstring Generate(const std::string& str)
		{
			std::wstring temp;

			int SourceLen = str.length();
			int StringLength = UTF8_TO_UNICODE::ConvertedLength(str.c_str(), SourceLen);

			int BufferSize = StringLength + 1;

			wchar_t* newWchar = new wchar_t[BufferSize];
			
			UTF8_TO_UNICODE::Convert(newWchar, BufferSize, str.c_str(), SourceLen + 1);
			
			temp = newWchar;
			delete[] newWchar;
			return temp;
		}
	}

	namespace UNICODE_TO_UTF8
	{
		template <typename BufferType>
		static int Utf8FromCodepoint(unsigned int Codepoint, BufferType OutputIterator, unsigned int OutputIteratorByteSizeRemaining)
		{
			// Ensure we have at least one character in size to write
			//checkSlow(OutputIteratorByteSizeRemaining >= sizeof(ANSICHAR));

			const BufferType OutputIteratorStartPosition = OutputIterator;

			if (Codepoint > 0x10FFFF)   // No Unicode codepoints above 10FFFFh, (for now!)
			{
				Codepoint = FMD_UNICODE_BOGUS_CHAR_CODEPOINT;
			}
			else if ((Codepoint == 0xFFFE) || (Codepoint == 0xFFFF))  // illegal values.
			{
				Codepoint = FMD_UNICODE_BOGUS_CHAR_CODEPOINT;
			}
			else if (FMD::IsHighSurrogate(Codepoint) || FMD::IsLowSurrogate(Codepoint)) // UTF-8 Characters are not allowed to encode codepoints in the surrogate pair range
			{
				Codepoint = FMD_UNICODE_BOGUS_CHAR_CODEPOINT;
			}

			// Do the encoding...
			if (Codepoint < 0x80)
			{
				*(OutputIterator++) = (char)Codepoint;
			}
			else if (Codepoint < 0x800)
			{
				if (OutputIteratorByteSizeRemaining >= 2)
				{
					*(OutputIterator++) = (char)((Codepoint >> 6) | 128 | 64);
					*(OutputIterator++) = (char)(Codepoint & 0x3F) | 128;
				}
				else
				{
					*(OutputIterator++) = (char)FMD_UNICODE_BOGUS_CHAR_CODEPOINT;
				}
			}
			else if (Codepoint < 0x10000)
			{
				if (OutputIteratorByteSizeRemaining >= 3)
				{
					*(OutputIterator++) = (char)((Codepoint >> 12) | 128 | 64 | 32);
					*(OutputIterator++) = (char)((Codepoint >> 6) & 0x3F) | 128;
					*(OutputIterator++) = (char)(Codepoint & 0x3F) | 128;
				}
				else
				{
					*(OutputIterator++) = (char)FMD_UNICODE_BOGUS_CHAR_CODEPOINT;
				}
			}
			else
			{
				if (OutputIteratorByteSizeRemaining >= 4)
				{
					*(OutputIterator++) = (char)((Codepoint >> 18) | 128 | 64 | 32 | 16);
					*(OutputIterator++) = (char)((Codepoint >> 12) & 0x3F) | 128;
					*(OutputIterator++) = (char)((Codepoint >> 6) & 0x3F) | 128;
					*(OutputIterator++) = (char)(Codepoint & 0x3F) | 128;
				}
				else
				{
					*(OutputIterator++) = (char)FMD_UNICODE_BOGUS_CHAR_CODEPOINT;
				}
			}

			return static_cast<int>(OutputIterator - OutputIteratorStartPosition);
		}

		template <typename DestBufferType>
		static void Convert_Impl(DestBufferType& Dest, int DestLen, const wchar_t* Source, const int SourceLen)
		{
			unsigned int HighCodepoint = FMD_MAX_uint32;

			for (int i = 0; i < SourceLen; ++i)
			{
				const bool bHighSurrogateIsSet = HighCodepoint != FMD_MAX_uint32;
				unsigned int Codepoint = static_cast<unsigned int>(Source[i]);

				// Check if this character is a high-surrogate
				if (FMD::IsHighSurrogate(Codepoint))
				{
					// Ensure we don't already have a high-surrogate set
					if (bHighSurrogateIsSet)
					{
						// Already have a high-surrogate in this pair
						// Write our stored value (will be converted into bogus character)
						if (!WriteCodepointToBuffer(HighCodepoint, Dest, DestLen))
						{
							// Could not write data, bail out
							return;
						}
					}

					// Store our code point for our next character
					HighCodepoint = Codepoint;
					continue;
				}

				// If our High Surrogate is set, check if this character is the matching low-surrogate
				if (bHighSurrogateIsSet)
				{
					if (FMD::IsLowSurrogate(Codepoint))
					{
						const unsigned int LowCodepoint = Codepoint;
						// Combine our high and low surrogates together to a single Unicode codepoint
						Codepoint = ((HighCodepoint - FMD::HIGH_SURROGATE_START_CODEPOINT) << 10) + (LowCodepoint - FMD::LOW_SURROGATE_START_CODEPOINT) + 0x10000;
					}
					else
					{
						// Did not find matching low-surrogate, write out a bogus character for our stored HighCodepoint
						if (!WriteCodepointToBuffer(HighCodepoint, Dest, DestLen))
						{
							// Could not write data, bail out
							return;
						}
					}

					// Reset our high-surrogate now that we've used (or discarded) its value
					HighCodepoint = FMD_MAX_uint32;
				}

				if (!WriteCodepointToBuffer(Codepoint, Dest, DestLen))
				{
					// Could not write data, bail out
					return;
				}
			}
		}

		static inline void Convert(char* Dest, const int DestLen, const wchar_t* Source, const int SourceLen)
		{
			Convert_Impl(Dest, DestLen, Source, SourceLen);
		}

		static inline int ConvertedLength(const wchar_t* Source, int SourceLen)
		{
			FMD::FMDCountingOutputIterator Dest;
			const int DestLen = SourceLen * 4;
			Convert_Impl(Dest, DestLen, Source, SourceLen);

			return Dest.GetCount();
		}

		template <typename DestBufferType>
		static bool WriteCodepointToBuffer(const unsigned int Codepoint, DestBufferType& Dest, int& DestLen)
		{
			int WrittenChars = Utf8FromCodepoint(Codepoint, Dest, DestLen);
			if (WrittenChars < 1)
			{
				return false;
			}

			Dest += WrittenChars;
			DestLen -= WrittenChars;
			return true;

		}
		const std::string Generate(const std::wstring& wstr)
		{
			std::string temp;

			int SourceLen = wstr.length();
			int StringLength = UNICODE_TO_UTF8::ConvertedLength(wstr.c_str(), SourceLen);

			int32 BufferSize = StringLength + 1;

			char* newTemp = new char[BufferSize];
			
			UNICODE_TO_UTF8::Convert(newTemp, BufferSize, wstr.c_str(), SourceLen + 1);

			temp = newTemp;
			delete[] newTemp;
			return temp;
		}
	}
}
