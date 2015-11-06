#include "processor.h"
#include "assert.h"

static inline uint64_t max_int();



extern void conditional_move(uint32_t A, uint32_t B, uint32_t C, Reg_T r)
{

        uint32_t *value = UArray_at(r, C);

        uint32_t *holder;
        uint32_t *recipient;

        if (*value) {
                holder = UArray_at(r, B);
        } else {
                holder = UArray_at(r, A);
        }
        
        recipient = UArray_at(r, A);
        *recipient = *holder;

}

extern void addition(uint32_t A, uint32_t B, uint32_t C, Reg_T r)
{

        uint32_t *holder = UArray_at(r, B);
        uint32_t value_B = *holder;

        holder = UArray_at(r, C);
        uint32_t value_C = *holder;
        holder = UArray_at(r, A);
        *holder = (value_B + value_C) % max_int();

}

extern void subtraction(uint32_t A, uint32_t B, uint32_t C, Reg_T r)
{

        uint32_t *holder = UArray_at(r, B);
        uint32_t value_B = *holder;

        holder = UArray_at(r, C);
        uint32_t value_C = *holder;

        holder = UArray_at(r, A);
        *holder = (value_B - value_C) % max_int();

}

extern void multiplication(uint32_t A, uint32_t B, uint32_t C, Reg_T r)
{

        uint32_t *holder = UArray_at(r, B);
        uint32_t value_B = *holder;

        holder = UArray_at(r, C);
        uint32_t value_C = *holder;

        holder = UArray_at(r, A);
        *holder = (value_B * value_C) % max_int();

}

extern void division(uint32_t A, uint32_t B, uint32_t C, Reg_T r)
{

        uint32_t *holder = UArray_at(r, B);
        uint32_t value_B = *holder;

        holder = UArray_at(r, C);
        uint32_t value_C = *holder;

        holder = UArray_at(r, A);
        *holder = (value_B / value_C) % max_int();

}

static inline uint64_t max_int()
{
	uint64_t one = 1;
	return ((one << 32) - 1);
}

extern void bitwise_nand(uint32_t A, uint32_t B, uint32_t C, Reg_T r)
{
        uint32_t *holder = UArray_at(r, B);
        uint32_t value_B = *holder;

        holder = UArray_at(r, C);
        uint32_t value_C = *holder;

        holder = UArray_at(r, A);
        *holder = ~(value_C & value_B);

}

extern void output(uint32_t C, Reg_T r)
{
        uint32_t *holder = UArray_at(r, C);

        assert(*holder <= 255);

        fprintf(stdout, "%d", *holder);

}

extern void input(uint32_t C, Reg_T r)
{
	/* NEEDS END OF FILE CASE */
	unsigned inp = fgetc(stdin);
	assert(inp <= 255);	
        uint32_t *holder = UArray_at(r, C);
	*holder = inp;
}

extern void load_value(uint32_t A, uint32_t value, Reg_T r)
{
        uint32_t *holder = UArray_at(r, A);

        *holder = value;

}

extern void halt(void)
{

        exit(0);
        
}
