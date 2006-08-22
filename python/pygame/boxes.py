import pygame
from pygame.locals import *

class UpDownBox(pygame.sprite.Sprite):
    def __init__(self, color, initial_position):
        pygame.sprite.Sprite.__init__(self)
        self.image = pygame.Surface([15, 15])
        self.image.fill(color)
        self.rect = self.image.get_rect()
        self.rect.topleft = initial_position
        self.going_down = True # Start going downwards
        self.next_update_time = 0 # update() hasn't been called yet.

    def update(self, current_time, bottom):
        # Update every 10 milliseconds = 1/100th of a second.
        if self.next_update_time < current_time:

            # If we're at the top or bottom of the screen, switch directions.
            if self.rect.bottom == bottom - 1: self.going_down = False
            elif self.rect.top == 0: self.going_down = True
     
            # Move our position up or down by one pixel
            if self.going_down: self.rect.top += 1
            else: self.rect.top -= 1

            self.next_update_time = current_time + 10

if __name__=="__main__":
    pygame.init()
    boxes = []
    for color, location in [([255, 0, 0], [0, 0]),
                            ([0, 255, 0], [60, 60]),
                            ([0, 0, 255], [120, 120])]:
        boxes.append(UpDownBox(color, location))

    screen = pygame.display.set_mode([150, 150])
    while pygame.event.poll().type != KEYDOWN:
        screen.fill([0, 0, 0]) # blank the screen.

        # Save time by only calling this once
        time = pygame.time.get_ticks() 
        for b in boxes:
          b.update(time, 150)
          screen.blit(b.image, b.rect)

        pygame.display.update()
