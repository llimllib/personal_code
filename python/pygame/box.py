import pygame
from pygame.locals import *

class Box(pygame.sprite.Sprite):
    def __init__(self, color, initial_position):

        # All sprite classes should extend pygame.sprite.Sprite. This
        # gives you several important internal methods that you probably
        # don't need or want to write yourself. Even if you do rewrite
        # the internal methods, you should extend Sprite, so things like
        # isinstance(obj, pygame.sprite.Sprite) return true on it.
        pygame.sprite.Sprite.__init__(self)
      
        # Create the image that will be displayed and fill it with the
        # right color.
        self.image = pygame.Surface([15, 15])
        self.image.fill(color)

        # Make our top-left corner the passed-in location.
        self.rect = self.image.get_rect()
        self.rect.topleft = initial_position

if __name__=="__main__":
    pygame.init()
    screen = pygame.display.set_mode([150, 150])
    b = Box([255, 0, 0], [0, 0]) # Make the box red in the top left
    screen.blit(b.image, b.rect)
    pygame.display.update()
    while pygame.event.poll().type != KEYDOWN: pygame.time.delay(10)
