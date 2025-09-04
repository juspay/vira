import { Page, expect } from "@playwright/test";

export class MainPage {
  constructor(private page: Page) {}

  async goto() {
    await this.page.goto("/");
  }

  async verifyPageLoaded() {
    await expect(this.page).toHaveURL("/");
    await expect(this.page).toHaveTitle(/Vira/);
  }

  async verifyNavigationLinks() {
    await expect(
      this.page.getByRole("link", { name: /repositories/i }),
    ).toBeVisible();
    await expect(
      this.page.getByRole("link", { name: /settings/i }),
    ).toBeVisible();
  }

  async navigateToRepositories() {
    await this.page.getByRole("link", { name: /repositories/i }).click();
  }
}
