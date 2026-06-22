import { FileText, Mail, Upload, User } from "lucide-react";
import { FormEvent } from "react";
import { type AuthUser } from "wasp/auth";
import { Button } from "../../../client/components/ui/button";
import {
  Card,
  CardContent,
  CardHeader,
  CardTitle,
} from "../../../client/components/ui/card";
import { Input } from "../../../client/components/ui/input";
import { Label } from "../../../client/components/ui/label";
import { Textarea } from "../../../client/components/ui/textarea";
import { Breadcrumb } from "../../layout/Breadcrumb";
import { DefaultLayout } from "../../layout/DefaultLayout";

export function SettingsPage({ user }: { user: AuthUser }) {
  const handleSubmit = (event: FormEvent<HTMLFormElement>) => {
    // TODO implement
    event.preventDefault();
    alert("Not yet implemented");
  };

  return (
    <DefaultLayout user={user}>
      <div className="max-w-270 mx-auto">
        <Breadcrumb pageName="Settings" />

        <div className="grid grid-cols-5 gap-8">
          <div className="col-span-5 xl:col-span-3">
            <Card>
              <CardHeader>
                <CardTitle>Personal Information</CardTitle>
              </CardHeader>
              <CardContent>
                <form onSubmit={handleSubmit}>
                  <div className="mb-5.5 gap-5.5 flex flex-col sm:flex-row">
                    <div className="w-full sm:w-1/2">
                      <Label
                        htmlFor="full-name"
                        className="text-foreground mb-3 block text-sm font-medium"
                      >
                        Full Name
                      </Label>
                      <div className="relative">
                        <User className="text-muted-foreground left-4.5 absolute top-2 h-5 w-5" />
                        <Input
                          className="pl-11.5"
                          type="text"
                          name="fullName"
                          id="full-name"
                          placeholder="Devid Jhon"
                          defaultValue="Devid Jhon"
                        />
                      </div>
                    </div>

                    <div className="w-full sm:w-1/2">
                      <Label
                        htmlFor="phone-number"
                        className="text-foreground mb-3 block text-sm font-medium"
                      >
                        Phone Number
                      </Label>
                      <Input
                        type=""
                        name="phoneNumber"
                        id="phone-number"
                        placeholder="+990 3343 7865"
                        defaultValue="+990 3343 7865"
                      />
                    </div>
                  </div>

                  <div className="mb-5.5">
                    <Label
                      htmlFor="email-address"
                      className="text-foreground mb-3 block text-sm font-medium"
                    >
                      Email Address
                    </Label>
                    <div className="relative">
                      <Mail className="text-muted-foreground left-4.5 absolute top-2 h-5 w-5" />
                      <Input
                        className="pl-11.5"
                        type="email"
                        name="emailAddress"
                        id="email-address"
                        placeholder="devidjond45@gmail.com"
                        defaultValue="devidjond45@gmail.com"
                      />
                    </div>
                  </div>

                  <div className="mb-5.5">
                    <Label
                      htmlFor="username"
                      className="text-foreground mb-3 block text-sm font-medium"
                    >
                      Username
                    </Label>
                    <Input
                      type="text"
                      name="Username"
                      id="username"
                      placeholder="devidjhon24"
                      defaultValue="devidjhon24"
                    />
                  </div>

                  <div className="mb-5.5">
                    <Label
                      htmlFor="bio"
                      className="text-foreground mb-3 block text-sm font-medium"
                    >
                      BIO
                    </Label>
                    <div className="relative">
                      <FileText className="text-muted-foreground left-4.5 absolute top-4 h-5 w-5" />
                      <Textarea
                        className="border-border bg-background text-foreground focus:border-primary pr-4.5 pl-11.5 focus-visible:outline-hidden w-full rounded border py-3"
                        name="bio"
                        id="bio"
                        rows={6}
                        placeholder="Write your bio here"
                        defaultValue="Lorem ipsum dolor sit amet, consectetur adipiscing elit. Pellentesque posuere fermentum urna, eu condimentum mauris tempus ut. Donec fermentum blandit aliquet."
                      ></Textarea>
                    </div>
                  </div>

                  <div className="gap-4.5 flex justify-end">
                    <Button variant="outline" type="submit">
                      Cancel
                    </Button>
                    <Button type="submit">Save</Button>
                  </div>
                </form>
              </CardContent>
            </Card>
          </div>
          <div className="col-span-5 xl:col-span-2">
            <Card>
              <CardHeader>
                <CardTitle>Your Photo</CardTitle>
              </CardHeader>
              <CardContent>
                <form action="#">
                  <div className="mb-4 flex items-center gap-3">
                    <div className="h-14 w-14 rounded-full">
                      {/* <img src={userThree} alt="User" /> */}
                    </div>
                    <div>
                      <span className="text-foreground mb-1.5">
                        Edit your photo
                      </span>
                      <span className="flex gap-2.5">
                        <button className="hover:text-primary text-sm">
                          Delete
                        </button>
                        <button className="hover:text-primary text-sm">
                          Update
                        </button>
                      </span>
                    </div>
                  </div>

                  <div
                    id="FileUpload"
                    className="border-primary bg-background mb-5.5 sm:py-7.5 relative block w-full cursor-pointer appearance-none rounded border-2 border-dashed px-4 py-4"
                  >
                    <input
                      type="file"
                      accept="image/*"
                      className="outline-hidden absolute inset-0 z-50 m-0 h-full w-full cursor-pointer p-0 opacity-0"
                    />
                    <div className="flex flex-col items-center justify-center space-y-3">
                      <span className="border-border bg-background flex h-10 w-10 items-center justify-center rounded-full border">
                        <Upload className="text-primary h-4 w-4" />
                      </span>
                      <p>
                        <span className="text-primary">Click to upload</span> or
                        drag and drop
                      </p>
                      <p className="mt-1.5">SVG, PNG, JPG or GIF</p>
                      <p>(max, 800 X 800px)</p>
                    </div>
                  </div>

                  <div className="gap-4.5 flex justify-end">
                    <Button variant="outline" type="submit">
                      Cancel
                    </Button>
                    <Button type="submit">Save</Button>
                  </div>
                </form>
              </CardContent>
            </Card>
          </div>
        </div>
      </div>
    </DefaultLayout>
  );
}
