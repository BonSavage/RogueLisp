#include "window.h"
#include <algorithm>
#include <iostream>

SDLWindow* SDLWindow::instance_; //Yes, we must declare it to avoid linking errors. Just like in C!

const u8 background_pixel = 0x00;
const u8 glyph_pixel = 0xff;
using uchar = unsigned char;
const char* font_location = "media\\font.bmp";

void SDLWindow::init_drawer(void)
{
    SDL_Surface* raw_font = SDL_LoadBMP(font_location);

    if (raw_font == nullptr)
        abort();

    assert((raw_font->h % cell_height) == 0 && (raw_font->w % cell_width) == 0);

    uchar sym_code = 0;

    for (size_t y = 0; y < raw_font->h; y += cell_height) { //iterate each raw
        for (size_t x = 0; x < raw_font->w; x += cell_width, ++sym_code) { //iterate each symbol of the raw
            compressed_pixels* sym_info = &bitmap_font[sym_code];

            u8* pixels = (u8*)(raw_font->pixels) + y*raw_font->w + x;

            size_t i = 0;

            auto func = [&pixels,&i,&raw_font](bit_iterator::bit b)-> void {
                b = pixels[i++];
                if (i == cell_width) {
                    pixels += raw_font->w;
                    i = 0;
                }
            };
            std::for_each(sym_info->begin(), sym_info->end(), func);

        }
    }
    SDL_FreeSurface(raw_font);
}

void SDLRender::draw_pixel(SDL_Point pos)
{
    SDL_RenderDrawPoint(renderer_, pos.x, pos.y);
}

void SDLRender::set_draw_color(SDL_Color c)
{
    SDL_SetRenderDrawColor(renderer_, c.r, c.g, c.b, c.a);
}

void SDLRender::clear(void)
{
    SDL_SetRenderDrawColor(renderer_, 0, 0, 0, 0);
    SDL_RenderClear(renderer_);
}

SDL_Renderer* SDLRender::renderer_create(SDL_Window* window)
{
    SDL_Renderer* renderer = SDL_CreateRenderer(window, -1, SDL_RENDERER_SOFTWARE);

    if (!renderer || !window)
        throw std::runtime_error("Cannot initialize game window");

    SDL_SetRenderDrawColor(renderer, 0, 0, 0, 0);

    SDL_RenderClear(renderer);

    return renderer;
}

using std::end,std::begin;

void SDLWindow::draw_gramma(const gramma gramma, const cell_coordinates c)
{
    compressed_pixels& gramma_shape = bitmap_font[gramma.code];
    SDL_Point base = {c.x*cell_width,c.y*cell_height};
    SDL_Point offset = { 0,0 };
    set_draw_color(SDL_Color{gramma.red,gramma.green,gramma.blue,0});

    for(bool b : gramma_shape) {

        if (b == true) {
            draw_pixel({base.x + offset.x,base.y+offset.y});
        }

        offset.x += 1;
        if (offset.x == cell_width) {
            offset.x = 0;
            offset.y += 1;
        }
    }
}

void SDLWindow::fill_rectangle(const rectangle rect,const color clr)
{
    SDLRender::fill_rectangle(SDL_Rect{rect.start.x*cell_width,
                            rect.start.y*cell_height,
                            (rect.size.x)*cell_width,
                            (rect.size.y)*cell_height}
                   ,SDL_Color{clr.red,clr.green,clr.blue,0});
}

void SDLWindow::fill_cell(const cell_coordinates cell,const color clr)
{
    fill_rectangle({cell,{1,1}},clr);
}

void SDLWindow::draw_frame(const rectangle rect, const color clr)
{
    const cell_coordinates start = rect.start;
    const cell_coordinates end = rect.start+rect.size-cell_coordinates{1,1};
    const cell_coordinates left_lower = start + cell_coordinates{static_cast<Uint8>(0),static_cast<Uint8>(end.y-start.y)}
                                    ,right_upper = start + cell_coordinates{static_cast<Uint8>(end.x-start.x),static_cast<Uint8>(0)};

    draw_gramma(make_gramma(201,clr),start);
    draw_gramma(make_gramma(188,clr),end);
    draw_gramma(make_gramma(200,clr),left_lower);
    draw_gramma(make_gramma(187,clr),right_upper);

    const gramma vertical = make_gramma(186,clr), horizontal = make_gramma(205,clr);

    for(short i = start.x+1;i < end.x;++i) {
        draw_gramma(horizontal,cell_coordinates(i,start.y));
        draw_gramma(horizontal,cell_coordinates(i,end.y));
    }

    for(short j = start.y+1;j < end.y;++j) {
        draw_gramma(vertical,cell_coordinates(start.x,j));
        draw_gramma(vertical,cell_coordinates(end.x,j));
    }
}

SDL_Window* SDLWindow::win_create(void)
{
    return SDL_CreateWindow("roguelike",
        SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED,
        width_*cell_width, height_*cell_height,
        SDL_WINDOW_MINIMIZED);
}

void SDLRender::fill_rectangle(SDL_Rect rect,SDL_Color clr)
{
    set_draw_color(clr);
    SDL_RenderFillRect(renderer_,&rect);
}

gramma make_gramma(const Uint8 code, const color clr)
{
    return {code,clr.red,clr.green,clr.blue};
}

cell_coordinates operator+(cell_coordinates p1,cell_coordinates p2) //C-style
{
    return {p1.x+p2.x,p1.y+p2.y};
}

cell_coordinates operator-(cell_coordinates p1,cell_coordinates p2)
{
    return {p1.x-p2.x,p1.y-p2.y};
}
